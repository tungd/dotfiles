;;; tterm-mode.el --- Major mode for tterm -*- lexical-binding: t; -*-

;; Redraw polling, keymaps, resize hooks, and mode setup.

(eval-and-compile
  (defconst tterm-mode--directory
    (file-name-directory (or load-file-name buffer-file-name default-directory))
    "Directory containing tterm mode Lisp files."))

(require 'cl-lib)
(require 'subr-x)
(require 'tterm-copy-mode (expand-file-name "tterm-copy-mode" tterm-mode--directory))
(require 'tterm-input (expand-file-name "tterm-input" tterm-mode--directory))
(require 'tterm-osc (expand-file-name "tterm-osc" tterm-mode--directory))

(defvar tterm-buffer-size)
(defvar tterm-capture-refresh-idle-interval)
(defvar tterm-fontset-fallbacks)
(defvar tterm-redraw-active-grace-delay)
(defvar tterm-redraw-idle-delay)
(defvar tterm-redraw-update-delay)
(defvar tterm-resize-debounce-delay)
(defvar tterm-resize-while-minibuffer-active)
(defvar tterm-wheel-scroll-lines)
(defvar tterm--alt-screen)
(defvar tterm--cursor-col)
(defvar tterm--cursor-row)
(defvar tterm--cursor-shape)
(defvar tterm--cursor-visible)
(defvar tterm--input-mode)
(defvar tterm--last-redraw-change-time)
(defvar tterm--last-capture-refresh-time)
(defvar tterm--mode-line-input-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'tterm-toggle-copy-mode)
    map)
  "Keymap for tterm's compact mode-line mode lighter.")
(defvar tterm--osc-indicator)
(defvar tterm--redraw-request-timer)
(defvar tterm--redraw-timer)
(defvar tterm--resize-timer)
(defvar tterm--resize-window-starts)
(defvar tterm--terminal)
(defvar tterm--title)

(declare-function tterm-alt-screen "tterm" (term))
(declare-function tterm-cols "tterm" (term))
(declare-function tterm-displayed-version "tterm" (term))
(declare-function tterm-id "tterm" (term))
(declare-function tterm-rows "tterm" (term))
(declare-function tterm-set-alt-screen "tterm" (term value))
(declare-function tterm--apply-op-data "tterm-apply" (id ops))
(declare-function tterm-bridge-command "tterm-bridge" (id command &optional payload))
(declare-function tterm--detach-current-terminal "tterm" ())
(declare-function tterm--effective-fontset-fallbacks "tterm" ())
(declare-function tterm--fontset-fallback-install-order "tterm" (fallbacks))
(declare-function tterm--initialize-screen "tterm-apply" (rows cols))
(declare-function tterm--mode-line-attention-indicator "tterm" ())
(declare-function tterm-header-line-format "tterm-osc" ())
(declare-function tterm--mode-line-osc-indicator "tterm-osc" ())
(declare-function tterm--pull-apply-plan-ops "tterm" (id displayed-version))
(declare-function tterm--resize "tterm" (id rows cols))
(declare-function tterm--sync-point-to-cursor "tterm-apply" ())
(declare-function tterm-jump-next-notification "tterm" ())

;;; Redraw polling

(defun tterm--redraw-now ()
  "Immediately pull pending apply ops and apply them to the current buffer.
Return non-nil when the pull found terminal changes."
  (let ((term tterm--terminal))
    (when (and term (not tterm--copy-mode))
      (let* ((displayed-version (or (tterm-displayed-version term) 0))
             (response (tterm--pull-apply-plan-ops
                        (tterm-id term)
                        displayed-version))
             (_base-version (aref response 0))
             (target-version (aref response 1))
             (reset (aref response 2))
             (ops (aref response 3))
             (changed (or reset
                          (/= target-version displayed-version)
                          ops)))
        (when reset
          (tterm--initialize-screen (tterm-rows term) (tterm-cols term)))
        (when ops
          (tterm--apply-op-data (tterm-id term) ops))
        (setf (tterm-displayed-version term) target-version)
        changed))))

(defun tterm--capture-refresh-due-p ()
  "Return non-nil when the current buffer should ask tmux for a pane snapshot."
  (let ((now (float-time)))
    (and (or (not tterm--last-capture-refresh-time)
             (>= (- now tterm--last-capture-refresh-time)
                 tterm-capture-refresh-idle-interval))
         now)))

(defun tterm--request-capture-refresh (&optional force)
  "Refresh the backend terminal state from tmux capture-pane.
When FORCE is nil, throttle requests by
`tterm-capture-refresh-idle-interval'."
  (when-let* ((term tterm--terminal)
              (now (or force (tterm--capture-refresh-due-p))))
    (let ((result (tterm-bridge-command (tterm-id term) "refresh-capture" "")))
      (when (and (stringp result) (not (string-empty-p result)))
        (setq-local tterm--last-capture-refresh-time
                    (if (numberp now) now (float-time)))
        t))))

(defun tterm--redraw-now-full ()
  "Redraw, using tmux capture-pane as an idle resync fallback."
  (if tterm--copy-mode
      nil
    (let ((changed (tterm--redraw-now)))
      (if changed
          changed
        (when (tterm--request-capture-refresh)
          (or (tterm--redraw-now) t))))))

(defun tterm--redraw-latest-snapshot ()
  "Force a full repaint from the latest backend terminal snapshot."
  (let ((term tterm--terminal))
    (when (and term (not tterm--copy-mode))
      (tterm--preserve-window-starts-or-tail
        (tterm--request-capture-refresh t)
        (let* ((response (tterm--pull-apply-plan-ops (tterm-id term) -1))
               (target-version (aref response 1))
               (ops (aref response 3)))
          (when (or (aref response 2) ops)
            (tterm--initialize-screen (tterm-rows term) (tterm-cols term)))
          (when ops
            (tterm--apply-op-data (tterm-id term) ops))
          (setf (tterm-displayed-version term) target-version)
          t)))))

(defun tterm-redraw ()
  "Force a full repaint of the current tterm buffer."
  (interactive)
  (unless (eq major-mode 'tterm-mode)
    (user-error "Not in a tterm buffer"))
  (unless tterm--terminal
    (user-error "No terminal attached"))
  (when tterm--copy-mode
    (user-error "Exit copy mode before forcing a redraw"))
  (when font-lock-mode
    (font-lock-mode -1))
  (tterm--resize-window)
  (tterm--redraw-latest-snapshot))

(defun tterm-redraw-all ()
  "Force a full repaint of all live tterm buffers.
Buffers in copy mode are left untouched."
  (interactive)
  (let ((count 0))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (eq major-mode 'tterm-mode)
                   tterm--terminal
                   (not tterm--copy-mode))
          (tterm-redraw)
          (setq count (1+ count)))))
    (when (called-interactively-p 'interactive)
      (message "Redrew %d tterm buffer%s" count (if (= count 1) "" "s")))
    count))

(defun tterm--redraw-active-p ()
  "Return non-nil when the current tterm buffer should poll for redraws."
  (and tterm--terminal
       (not tterm--copy-mode)
       (let ((window (get-buffer-window (current-buffer) t)))
         (and window
              (or (not (display-graphic-p (window-frame window)))
                  (frame-visible-p (window-frame window)))))))

(defun tterm--update-redraw-timer ()
  "Start or stop the current tterm buffer's redraw timer as needed."
  (when (eq major-mode 'tterm-mode)
    (cond
     ((not (tterm--redraw-active-p))
      (tterm--stop-redraw-timer)
      (tterm--stop-redraw-request-timer)
      (tterm--stop-resize-timer))
     ((tterm--resize-pending-p)
      (tterm--stop-redraw-timer)
      (tterm--stop-redraw-request-timer))
     (t
      (tterm--start-redraw-timer)))))

(defun tterm--update-redraw-timers (&rest _)
  "Start or stop redraw timers for all live tterm buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'tterm-mode)
        (tterm--update-redraw-timer)))))

(defun tterm--redraw-delay (changed)
  "Return the next redraw delay after a pull with CHANGED result."
  (when changed
    (setq-local tterm--last-redraw-change-time (float-time)))
  (let ((recently-changed
         (and tterm--last-redraw-change-time
              (< (- (float-time) tterm--last-redraw-change-time)
                 tterm-redraw-active-grace-delay))))
    (max 0.001
         (if (or changed recently-changed)
             tterm-redraw-update-delay
           tterm-redraw-idle-delay))))

(defun tterm--start-redraw-timer ()
  "Start periodic apply-op pulls for the current tterm buffer."
  (when (and (not (timerp tterm--redraw-timer))
             (not (tterm--resize-pending-p))
             (tterm--redraw-active-p))
    (tterm--schedule-redraw-timer (tterm--redraw-delay t))))

(defun tterm--schedule-redraw-timer (delay)
  "Schedule the next redraw poll for the current tterm buffer after DELAY."
  (when (and (not (timerp tterm--redraw-timer))
             (tterm--redraw-active-p))
    (let ((buffer (current-buffer))
          timer)
      (setq tterm--redraw-timer
            (setq timer
                  (run-at-time
                   delay nil
                   (lambda ()
                     (if (not (buffer-live-p buffer))
                         (cancel-timer timer)
                       (with-current-buffer buffer
                         (when (eq tterm--redraw-timer timer)
                           (setq tterm--redraw-timer nil))
                         (when (tterm--redraw-active-p)
                           (let ((changed (tterm--redraw-now-unless-resizing)))
                             (unless (tterm--resize-pending-p)
                               (tterm--schedule-redraw-timer
                                (tterm--redraw-delay changed))))))))))))))

(defun tterm--stop-redraw-timer ()
  "Stop periodic apply-op pulls for the current tterm buffer."
  (when (timerp tterm--redraw-timer)
    (cancel-timer tterm--redraw-timer)
    (setq tterm--redraw-timer nil)))

(defun tterm--stop-redraw-request-timer ()
  "Stop the pending one-shot redraw request timer."
  (when (timerp tterm--redraw-request-timer)
    (cancel-timer tterm--redraw-request-timer)
    (setq tterm--redraw-request-timer nil)))

(defun tterm--stop-resize-timer (&optional keep-window-starts)
  "Stop the pending coalesced resize timer."
  (when (timerp tterm--resize-timer)
    (cancel-timer tterm--resize-timer)
    (setq tterm--resize-timer nil))
  (unless keep-window-starts
    (setq-local tterm--resize-window-starts nil)))

(defun tterm--resize-pending-p ()
  "Return non-nil while automatic resize or resize redraw settle is pending."
  (timerp tterm--resize-timer))

(remove-function after-focus-change-function #'tterm--update-redraw-timers)
(add-function :after after-focus-change-function #'tterm--update-redraw-timers)
(add-hook 'window-state-change-functions #'tterm--update-redraw-timers)

(defun tterm--kill-buffer ()
  "Kill the tmux window for the current buffer."
  (tterm--kill-current-terminal-window))

;;; Mouse and input-mode handling

(defun tterm--scrollback-scroll (command)
  "Run scroll COMMAND, ignoring buffer-limit scroll errors."
  (condition-case nil
      (funcall command (max 1 tterm-wheel-scroll-lines))
    (beginning-of-buffer nil)
    (end-of-buffer nil)))

(defun tterm--scrollback-command-scroll (command arg)
  "Run scroll COMMAND with prefix ARG, ignoring buffer-limit scroll errors."
  (condition-case nil
      (let ((current-prefix-arg arg))
        (call-interactively command))
    (beginning-of-buffer nil)
    (end-of-buffer nil)))

(defun tterm--scrollback-command-up (&optional arg)
  "Scroll toward older terminal output for generic scroll commands."
  (interactive "P")
  (let ((was-copy-mode tterm--copy-mode))
    (unless tterm--copy-mode
      (tterm-copy-mode))
    (if (> tterm--copy-mode-scrollback-rows 0)
        (tterm--scrollback-command-scroll #'scroll-down-command arg)
      (unless was-copy-mode
        (tterm-copy-mode-exit)))))

(defun tterm--scrollback-command-down (&optional arg)
  "Scroll toward newer terminal output for generic scroll commands."
  (interactive "P")
  (when tterm--copy-mode
    (tterm--scrollback-command-scroll #'scroll-up-command arg)))

(defun tterm--mouse-event-terminal-position (event)
  "Return EVENT's terminal position as a 1-indexed (COL . ROW) pair."
  (let* ((term tterm--terminal)
         (posn (and event (event-start event)))
         (point (and posn (posn-point posn)))
         (row nil)
         (col nil))
    (when (integer-or-marker-p point)
      (save-excursion
        (goto-char point)
        (setq row (1- (line-number-at-pos point t)))
        (setq col (current-column))))
    (unless row
      (setq row tterm--cursor-row))
    (unless col
      (setq col tterm--cursor-col))
    (setq row (max 0 (min (1- (or (and term (tterm-rows term)) 1)) row)))
    (setq col (max 0 (min (1- (or (and term (tterm-cols term)) 1)) col)))
    (cons (1+ col) (1+ row))))

(defun tterm--send-wheel-mouse-event (button event)
  "Send xterm SGR wheel BUTTON for mouse wheel EVENT to the terminal."
  (let ((pos (tterm--mouse-event-terminal-position event)))
    (tterm--send-key (format "\e[<%d;%d;%dM" button (car pos) (cdr pos)))))

(defun tterm--parse-pane-alt-screen (text)
  "Return pane alt-screen state parsed from pane-state TEXT."
  (catch 'state
    (dolist (line (split-string (or text "") "\n" t) 'unknown)
      (pcase (split-string line "\t")
        (`("alt" ,value)
         (throw 'state (string= value "1")))))))

(defun tterm--sync-pane-alt-screen ()
  "Sync cached alt-screen state from tmux pane metadata."
  (when tterm--terminal
    (let* ((text (ignore-errors
                   (tterm-bridge-command
                    (tterm-id tterm--terminal) "pane-state" "")))
           (alt-screen (tterm--parse-pane-alt-screen text)))
      (unless (eq alt-screen 'unknown)
        (setq-local tterm--alt-screen alt-screen)
        (tterm-set-alt-screen tterm--terminal alt-screen))
      (and (not (eq alt-screen 'unknown)) alt-screen))))

(defun tterm--send-alt-screen-wheel (button event)
  "Send alternate-screen wheel BUTTON for EVENT when applicable."
  (let ((term tterm--terminal))
    (when term
      (tterm--sync-pane-alt-screen))
    (when (and term (tterm-alt-screen term))
      (tterm--send-wheel-mouse-event button event)
      t)))

(defun tterm--wheel-up (&optional event)
  "Scroll toward older terminal output for mouse wheel EVENT."
  (interactive "e")
  (unless (tterm--send-alt-screen-wheel 64 event)
    (let ((was-copy-mode tterm--copy-mode))
      (unless tterm--copy-mode
        (tterm-copy-mode))
      (if (> tterm--copy-mode-scrollback-rows 0)
          (tterm--scrollback-scroll #'scroll-down-line)
        (unless was-copy-mode
          (tterm-copy-mode-exit))))))

(defun tterm--wheel-down (&optional event)
  "Scroll toward newer terminal output for mouse wheel EVENT."
  (interactive "e")
  (unless (tterm--send-alt-screen-wheel 65 event)
    (when tterm--copy-mode
      (tterm--scrollback-scroll #'scroll-up-line))))

(defun tterm--refresh-mouse-posn (posn)
  "Return POSN recalculated from its window and pixel coordinates."
  (let* ((window (and posn (posn-window posn)))
         (xy (and posn (posn-x-y posn))))
    (if (and (windowp window) (consp xy))
        (or (posn-at-x-y (car xy) (cdr xy) window) posn)
      posn)))

(defun tterm--refresh-mouse-event-position (event)
  "Return EVENT with its position data recalculated in the current window."
  (let ((updated (copy-tree event)))
    (when (consp (cdr updated))
      (setcar (cdr updated)
              (tterm--refresh-mouse-posn (event-start event))))
    (when (consp (cddr updated))
      (setcar (cddr updated)
              (tterm--refresh-mouse-posn (event-end event))))
    updated))

(defun tterm--mouse-drag-region (event)
  "Enter copy mode, then delegate mouse selection for EVENT."
  (interactive "e")
  (unless tterm--copy-mode
    (tterm-copy-mode)
    (redisplay t)
    (setq event (tterm--refresh-mouse-event-position event)))
  (mouse-drag-region event))

(defun tterm--mouse-ignore-down (_event)
  "Ignore mouse-down events so plain clicks do not start selection tracking."
  (interactive "e"))

(defun tterm--mouse-set-point (event)
  "Enter copy mode if needed, refresh EVENT positions, then set point."
  (interactive "e")
  (unless tterm--copy-mode
    (tterm-copy-mode)
    (redisplay t))
  (mouse-set-point (tterm--refresh-mouse-event-position event)))

(defun tterm--mouse-set-region (event)
  "Enter copy mode if needed, refresh EVENT positions, then select region."
  (interactive "e")
  (unless tterm--copy-mode
    (tterm-copy-mode)
    (redisplay t))
  (mouse-set-region (tterm--refresh-mouse-event-position event)))

(defun tterm-toggle-copy-mode ()
  "Toggle the current tterm buffer between normal and copy modes."
  (interactive)
  (unless (eq major-mode 'tterm-mode)
    (user-error "Not in a tterm buffer"))
  (if tterm--copy-mode
      (tterm-normal-mode)
    (tterm-copy-mode)))

(defun tterm--mode-line-mode-letter ()
  "Return a colored compact input-mode letter for the mode line."
  (pcase (if tterm--copy-mode 'copy tterm--input-mode)
    ('copy (propertize "C" 'face 'tterm-mode-line-copy))
    ('normal (propertize "N" 'face 'tterm-mode-line-normal))
    (_ (propertize "?" 'face 'shadow))))

(defun tterm--mode-line-mode-name ()
  "Return the tterm mode lighter with compact input-mode state."
  (let ((text (concat "tterm[" (tterm--mode-line-mode-letter) "]")))
    (add-text-properties
     0 (length text)
     `(local-map ,tterm--mode-line-input-map
       mouse-face mode-line-highlight
       help-echo "mouse-1: toggle tterm copy mode")
     text)
    text))

(defun tterm--mode-line-input-state ()
  "Return extra tterm state for `mode-line-format'."
  (concat (tterm--mode-line-osc-indicator)
          (if (fboundp 'tterm--mode-line-attention-indicator)
              (tterm--mode-line-attention-indicator)
            "")))

(defun tterm--set-local-map-for-mode ()
  "Set the local keymap based on copy/input mode state."
  (if tterm--copy-mode
      (use-local-map tterm-copy-mode-map)
    (use-local-map tterm-mode-map)))

(defun tterm--set-input-mode (mode)
  "Set terminal input MODE and update keymap + mode line."
  (unless (eq mode 'normal)
    (user-error "Unknown tterm input mode: %S" mode))
  (setq-local tterm--input-mode mode)
  (tterm--set-local-map-for-mode)
  (force-mode-line-update))

(defun tterm-normal-mode ()
  "Enable normal terminal input for the current tterm buffer."
  (interactive)
  (unless (eq major-mode 'tterm-mode)
    (user-error "Not in a tterm buffer"))
  (when tterm--copy-mode
    (tterm--restore-copy-mode-display)
    (setq-local tterm--copy-mode nil)
    (tterm--redraw-latest-snapshot)
    (tterm--show-copy-mode-live-screen tterm--cursor-row tterm--cursor-col))
  (setq-local tterm--copy-mode nil)
  (setq-local buffer-read-only t)
  (tterm--start-redraw-timer)
  (tterm--set-input-mode 'normal))

;;; Resize handling

(defun tterm--resize-suppressed-p ()
  "Return non-nil when a window resize should not resize the PTY."
  (and (not tterm-resize-while-minibuffer-active)
       (active-minibuffer-window)))

(defun tterm--window-grid-size (&optional window)
  "Return terminal grid size for WINDOW as (ROWS . COLS)."
  (let ((window (or window (selected-window))))
    (if (and (display-graphic-p (window-frame window))
             (fboundp 'window-font-width)
             (fboundp 'window-default-line-height))
        (let* ((pixel-width (window-body-width window t))
               (pixel-height (window-body-height window t))
               (cell-width (max 1 (window-font-width window)))
               (cell-height (max 1 (window-default-line-height window)))
               (cols (max 1 (/ pixel-width cell-width)))
               (rows (max 1 (/ pixel-height cell-height))))
          (cons rows cols))
      (cons (window-body-height window)
            (window-body-width window)))))

(defun tterm--clamp-cursor-to-grid (rows cols)
  "Clamp the cached terminal cursor to ROWS x COLS."
  (setq-local tterm--cursor-row (min (max 0 tterm--cursor-row)
                                     (max 0 (1- rows)))
              ;; Cursor column can legitimately sit at COLS, i.e. line end.
              tterm--cursor-col (min (max 0 tterm--cursor-col)
                                     (max 0 cols))))

(defun tterm--visible-window-starts ()
  "Return visible tterm window starts for the current buffer."
  (mapcar (lambda (window)
            (list window (window-start window) nil))
          (get-buffer-window-list (current-buffer) nil t)))

(defun tterm--visible-window-freeze-state ()
  "Return visible tterm windows with raw starts for resize freeze."
  (mapcar (lambda (window)
            (list window (window-start window)))
          (get-buffer-window-list (current-buffer) nil t)))

(defun tterm--window-tail-pinned-p (window)
  "Return non-nil when WINDOW is showing the live buffer tail."
  (let ((end (window-end window t)))
    (and end (>= end (point-max)))))

(defun tterm--visible-window-starts-or-tail ()
  "Return visible tterm window starts, preserving tail-pinned windows as tails."
  (mapcar (lambda (window)
            (list window
                  (window-start window)
                  (tterm--window-tail-pinned-p window)))
          (get-buffer-window-list (current-buffer) nil t)))

(defun tterm--tail-window-start (window position)
  "Return a window start that keeps POSITION at the bottom of WINDOW."
  (save-excursion
    (goto-char (min (max position (point-min)) (point-max)))
    (forward-line (- 1 (max 1 (window-body-height window))))
    (line-beginning-position)))

(defun tterm--restore-window-starts (starts)
  "Restore visible window STARTS captured by `tterm--visible-window-starts'."
  (dolist (entry starts)
    (let ((window (nth 0 entry))
          (start (nth 1 entry))
          (tail-pinned (nth 2 entry)))
      (when (window-live-p window)
        (if tail-pinned
            (let ((target (point)))
              (set-window-point window target)
              (set-window-start
               window
               (tterm--tail-window-start window target)
               t))
          (set-window-start window start t))))))

(defun tterm--restore-resize-window-starts ()
  "Restore window starts captured for a pending automatic resize."
  (when-let* ((state (plist-get tterm--resize-window-starts :freeze)))
    (dolist (entry state)
      (let ((window (nth 0 entry))
            (start (nth 1 entry)))
        (when (window-live-p window)
          (let ((point (min (max start (point-min)) (point-max))))
            (set-window-point window point)
            (set-window-start window start t)))))))

(defmacro tterm--preserve-window-starts (&rest body)
  "Run BODY without letting cursor housekeeping move visible window starts."
  (declare (indent 0) (debug t))
  `(let ((starts (tterm--visible-window-starts)))
     (unwind-protect
         (progn ,@body)
       (tterm--restore-window-starts starts))))

(defmacro tterm--preserve-window-starts-or-tail (&rest body)
  "Run BODY preserving manual scrollback, but keeping tail-pinned windows live."
  (declare (indent 0) (debug t))
  `(let ((starts (tterm--visible-window-starts-or-tail)))
     (unwind-protect
         (progn ,@body)
       (tterm--restore-window-starts starts))))

(defun tterm--resize-window (&optional frame)
  "Resize terminal to match its displayed window on FRAME.
Return non-nil when a resize was sent."
  (tterm--preserve-window-starts-or-tail
    (let ((term tterm--terminal)
          (window (get-buffer-window (current-buffer) frame)))
      (when (and term window
                 (not tterm--copy-mode)
                 (not (tterm--resize-suppressed-p)))
        (let* ((grid (tterm--window-grid-size window))
               (rows (car grid))
               (cols (cdr grid)))
          (unless (and (= rows (tterm-rows term))
                       (= cols (tterm-cols term)))
            (tterm--clamp-cursor-to-grid rows cols)
            (tterm--sync-point-to-cursor)
            (setf (tterm-rows term) rows
                  (tterm-cols term) cols)
            (tterm--resize (tterm-id term) rows cols)
            t))))))

(defun tterm--resize-and-redraw-now (&optional frame)
  "Resize and redraw without moving visible terminal window starts."
  (tterm--preserve-window-starts-or-tail
    (tterm--resize-window frame)
    (tterm--redraw-now-full)))

(defun tterm--resize-needed-p (&optional frame)
  "Return non-nil when the displayed window size differs from the PTY size."
  (let ((term tterm--terminal)
        (window (get-buffer-window (current-buffer) frame)))
    (when (and term window
               (not tterm--copy-mode)
               (not (tterm--resize-suppressed-p)))
      (let* ((grid (tterm--window-grid-size window))
             (rows (car grid))
             (cols (cdr grid)))
        (not (and (= rows (tterm-rows term))
                  (= cols (tterm-cols term))))))))

(defun tterm--schedule-resize-window (&optional frame)
  "Coalesce automatic terminal resize requests for the current buffer."
  (when (eq major-mode 'tterm-mode)
    (if (not (tterm--resize-needed-p frame))
        (progn
          (tterm--stop-resize-timer)
          nil)
      (let ((buffer (current-buffer)))
        (unless tterm--resize-window-starts
          (setq-local tterm--resize-window-starts
                      (list :freeze (tterm--visible-window-freeze-state)
                            :final (tterm--visible-window-starts-or-tail))))
        (tterm--stop-resize-timer t)
        (tterm--stop-redraw-timer)
        (tterm--stop-redraw-request-timer)
        (setq-local
         tterm--resize-timer
         (run-at-time
          tterm-resize-debounce-delay nil
          (lambda ()
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (let ((final-starts (plist-get tterm--resize-window-starts
                                               :final)))
                  (setq-local tterm--resize-timer nil)
                  (unwind-protect
                      (when (tterm--redraw-active-p)
                        (tterm--resize-window frame)
                        (tterm--restore-resize-window-starts)
                        (tterm--redraw-latest-snapshot)
                        (when final-starts
                          (tterm--restore-window-starts final-starts))
                        (tterm--update-redraw-timer))
                    (setq-local tterm--resize-window-starts nil))))))))
        (tterm--restore-resize-window-starts)
        t))))

(defun tterm--redraw-now-unless-resizing ()
  "Redraw now unless an automatic resize is settling."
  (cond
   ((tterm--resize-pending-p)
    (tterm--restore-resize-window-starts)
    nil)
   ((tterm--schedule-resize-window) nil)
   (t (tterm--redraw-now-full))))

(defun tterm--post-command-sync-point ()
  "Keep point pinned to the terminal cursor in terminal input modes."
  (when (and (eq major-mode 'tterm-mode)
             (not tterm--copy-mode))
    (tterm--update-redraw-timer)
    (tterm--preserve-window-starts
      (cond
       ((tterm--resize-pending-p)
        (tterm--restore-resize-window-starts))
       ((tterm--schedule-resize-window) nil)
       (t (tterm--sync-point-to-cursor))))))

(defun tterm--install-buffer-hooks ()
  "Install current tterm local hooks, replacing stale resize hooks."
  (remove-hook 'window-size-change-functions #'tterm--resize-window t)
  (remove-hook 'window-size-change-functions #'tterm--schedule-resize-window t)
  (add-hook 'window-size-change-functions #'tterm--schedule-resize-window nil t)
  (remove-hook 'post-command-hook #'tterm--post-command-sync-point t)
  (add-hook 'post-command-hook #'tterm--post-command-sync-point nil t))

(defun tterm--install-default-face-remap ()
  "Render unstyled terminal cells with terminal default colors."
  (setq-local face-remapping-alist (copy-tree '((default tterm-default)))))

(defun tterm--refresh-live-buffer-hooks ()
  "Refresh local display state and hooks in live tterm buffers after code reload."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'tterm-mode)
        (setq-local tterm--input-mode 'normal)
        (tterm--install-default-face-remap)
        (tterm--install-buffer-hooks)
        (tterm--set-local-map-for-mode)
        (tterm--update-redraw-timer)))))

;;; Major mode

(defvar tterm-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Self-inserting keys
    (define-key map [remap self-insert-command] #'tterm--self-insert)
    ;; Control keys
    (define-key map (kbd "RET") #'tterm--newline)
    (define-key map (kbd "DEL") #'tterm--backspace)
    (define-key map (kbd "C-<backspace>") #'tterm--backward-kill-word)
    (define-key map (kbd "C-DEL") #'tterm--backward-kill-word)
    (define-key map (kbd "TAB") #'tterm--tab)
    (define-key map [escape] #'tterm--escape)
    (define-key map [remap yank] #'tterm--paste)
    (define-key map [remap clipboard-yank] #'tterm--paste)
    (define-key map [remap yank-media] #'tterm-paste-clipboard-media)
    (define-key map [remap scroll-down-command] #'tterm--scrollback-command-up)
    (define-key map [remap scroll-up-command] #'tterm--scrollback-command-down)
    (define-key map (kbd "C-v") #'tterm--paste)
    (define-key map (kbd "C-y") #'tterm--paste)
    (define-key map (kbd "s-v") #'tterm--paste)
    (define-key map [paste] #'tterm--paste)
    (define-key map (kbd "<delete>") #'tterm--delete)
    (define-key map [backtab] (lambda () (interactive) (tterm--send-special-key 'backtab)))
    (define-key map (kbd "<up>") (lambda () (interactive) (tterm--send-special-key 'up)))
    (define-key map (kbd "<down>") (lambda () (interactive) (tterm--send-special-key 'down)))
    (define-key map (kbd "<right>") (lambda () (interactive) (tterm--send-special-key 'right)))
    (define-key map (kbd "<left>") (lambda () (interactive) (tterm--send-special-key 'left)))
    (define-key map (kbd "<home>") (lambda () (interactive) (tterm--send-special-key 'home)))
    (define-key map (kbd "<end>") (lambda () (interactive) (tterm--send-special-key 'end)))
    (define-key map (kbd "<insert>") (lambda () (interactive) (tterm--send-special-key 'insert)))
    (define-key map (kbd "<prior>") (lambda () (interactive) (tterm--send-special-key 'prior)))
    (define-key map (kbd "<next>") (lambda () (interactive) (tterm--send-special-key 'next)))
    (define-key map (kbd "<f1>") (lambda () (interactive) (tterm--send-special-key 'f1)))
    (define-key map (kbd "<f2>") (lambda () (interactive) (tterm--send-special-key 'f2)))
    (define-key map (kbd "<f3>") (lambda () (interactive) (tterm--send-special-key 'f3)))
    (define-key map (kbd "<f4>") (lambda () (interactive) (tterm--send-special-key 'f4)))
    (define-key map (kbd "<f5>") (lambda () (interactive) (tterm--send-special-key 'f5)))
    (define-key map (kbd "<f6>") (lambda () (interactive) (tterm--send-special-key 'f6)))
    (define-key map (kbd "<f7>") (lambda () (interactive) (tterm--send-special-key 'f7)))
    (define-key map (kbd "<f8>") (lambda () (interactive) (tterm--send-special-key 'f8)))
    (define-key map (kbd "<f9>") (lambda () (interactive) (tterm--send-special-key 'f9)))
    (define-key map (kbd "<f10>") (lambda () (interactive) (tterm--send-special-key 'f10)))
    (define-key map (kbd "<f11>") (lambda () (interactive) (tterm--send-special-key 'f11)))
    (define-key map (kbd "<f12>") (lambda () (interactive) (tterm--send-special-key 'f12)))
    (define-key map [wheel-up] #'tterm--wheel-up)
    (define-key map [wheel-down] #'tterm--wheel-down)
    (define-key map [double-wheel-up] #'tterm--wheel-up)
    (define-key map [double-wheel-down] #'tterm--wheel-down)
    (define-key map [triple-wheel-up] #'tterm--wheel-up)
    (define-key map [triple-wheel-down] #'tterm--wheel-down)
    (define-key map [mouse-4] #'tterm--wheel-up)
    (define-key map [mouse-5] #'tterm--wheel-down)
    (define-key map [drag-n-drop] #'tterm--dnd-send-files)
    (define-key map [down-mouse-1] #'tterm--mouse-ignore-down)
    (define-key map [mouse-1] #'tterm--mouse-set-point)
    (define-key map [drag-mouse-1] #'tterm--mouse-set-region)
    ;; Common control keys
    (define-key map (kbd "C-^") #'tterm-copy-mode)
    (define-key map (kbd "C-c C-c") #'tterm-send-interrupt)
    (define-key map (kbd "C-c C-f") #'tterm-send-file)
    (define-key map (kbd "C-d") #'tterm-send-eof)
    (define-key map (kbd "C-c C-v") #'tterm-paste-clipboard-media)
    (define-key map (kbd "C-c C-o") #'tterm-open-hyperlink)

    (dolist (key '("A" "B" "D" "E" "F" "J" "K" "L" "N" "O" "P" "R" "T" "U" "W" "Z"))
      (define-key map
        (kbd (concat "C-" (downcase key)))
        (tterm--control-key-command key)))
    ;; Window change hook
    map)
  "Keymap for `tterm-mode'.")

;; Keep reloads from leaving existing sessions on stale global editing bindings.
(define-key tterm-mode-map (kbd "C-<backspace>") #'tterm--backward-kill-word)
(define-key tterm-mode-map (kbd "C-DEL") #'tterm--backward-kill-word)
(define-key tterm-mode-map [backtab] (lambda () (interactive) (tterm--send-special-key 'backtab)))
(define-key tterm-mode-map (kbd "C-j") (tterm--control-key-command "J"))
(define-key tterm-mode-map (kbd "C-c C-f") #'tterm-send-file)
(define-key tterm-mode-map (kbd "C-c C-n") #'tterm-jump-next-notification)
(define-key tterm-mode-map (kbd "C-c C-v") #'tterm-paste-clipboard-media)
(define-key tterm-mode-map (kbd "C-d") #'tterm-send-eof)
(define-key tterm-mode-map (kbd "C-l") (tterm--control-key-command "L"))
(define-key tterm-mode-map [remap scroll-down-command] #'tterm--scrollback-command-up)
(define-key tterm-mode-map [remap scroll-up-command] #'tterm--scrollback-command-down)
(define-key tterm-mode-map [double-wheel-up] #'tterm--wheel-up)
(define-key tterm-mode-map [double-wheel-down] #'tterm--wheel-down)
(define-key tterm-mode-map [triple-wheel-up] #'tterm--wheel-up)
(define-key tterm-mode-map [triple-wheel-down] #'tterm--wheel-down)
(define-key tterm-mode-map [mouse-4] #'tterm--wheel-up)
(define-key tterm-mode-map [mouse-5] #'tterm--wheel-down)
(define-key tterm-mode-map [down-mouse-1] #'tterm--mouse-ignore-down)
(define-key tterm-mode-map [mouse-1] #'tterm--mouse-set-point)
(define-key tterm-mode-map [drag-mouse-1] #'tterm--mouse-set-region)

(defun tterm--cleanup-obsolete-input-mode-state ()
  "Remove stale three-mode symbols and bindings after source reload."
  (dolist (fn '(tterm-char-mode tterm-semi-char-mode tterm-cycle-input-mode))
    (when (fboundp fn)
      (fmakunbound fn)))
  (when (boundp 'tterm--char-mode-map)
    (makunbound 'tterm--char-mode-map))
  (define-key tterm-mode-map (kbd "C-]") nil)
  (define-key tterm--mode-line-input-map
              [mode-line mouse-1]
              #'tterm-toggle-copy-mode))

(tterm--cleanup-obsolete-input-mode-state)

(defun tterm--fontset-fallback-install-key (frame fallbacks)
  "Return the FRAME-local cache key for installed FALLBACKS."
  (list fallbacks
        (frame-parameter frame 'font)
        (face-attribute 'default :font frame 'default)))

(defun tterm--install-fontset-fallbacks (&optional frame force)
  "Install tterm's fallback fonts into FRAME's fontset."
  (let ((frame (or frame (selected-frame))))
    (when (and (display-graphic-p frame) tterm-fontset-fallbacks)
      (let ((fallbacks (tterm--effective-fontset-fallbacks))
            (installed (frame-parameter
                        frame 'tterm-fontset-fallbacks-installed)))
        (unless (and (not force)
                     (equal installed
                            (tterm--fontset-fallback-install-key
                             frame fallbacks)))
          (dolist (entry (tterm--fontset-fallback-install-order fallbacks))
            (set-fontset-font nil (car entry) (cdr entry) frame 'prepend))
          (set-frame-parameter frame 'tterm-fontset-fallbacks-installed
                               (tterm--fontset-fallback-install-key
                                frame fallbacks)))))))

(defun tterm--live-tterm-buffer-p ()
  "Return non-nil when any live buffer is in `tterm-mode'."
  (cl-some (lambda (buffer)
             (with-current-buffer buffer
               (eq major-mode 'tterm-mode)))
           (buffer-list)))

(defun tterm--refresh-fontset-fallbacks-after-font-change ()
  "Reinstall fallback fonts after Emacs rebuilds frame fontsets."
  (when (tterm--live-tterm-buffer-p)
    (dolist (frame (frame-list))
      (tterm--install-fontset-fallbacks frame t))))

(when (boundp 'after-setting-font-hook)
  (add-hook 'after-setting-font-hook
            #'tterm--refresh-fontset-fallbacks-after-font-change))

(define-derived-mode tterm-mode fundamental-mode "tterm"
  "Major mode for tterm terminal emulator."
  (when font-lock-mode
    (font-lock-mode -1))
  (setq-local tterm--terminal nil)
  (setq-local tterm--title nil)
  (setq-local tterm--redraw-timer nil)
  (setq-local tterm--resize-timer nil)
  (setq-local tterm--resize-window-starts nil)
  (setq-local tterm--last-redraw-change-time nil)
  (setq-local tterm--input-mode 'normal)
  (setq-local tterm--osc-indicator nil)
  (setq-local buffer-undo-list t)
  (setq-local scroll-conservatively 10000)
  (setq-local scroll-margin 0)
  (setq-local hscroll-margin 0)
  (setq-local hscroll-step 1)
  (setq-local truncate-lines t)
  (tterm--install-default-face-remap)
  (setq-local font-lock-defaults '(nil t))
  (setq-local bidi-paragraph-direction 'left-to-right)
  (tterm--install-fontset-fallbacks
   (when-let* ((window (get-buffer-window (current-buffer) t)))
     (window-frame window)))
  (when (boundp 'bidi-inhibit-bpa)
    (setq-local bidi-inhibit-bpa t))
  (setq-local show-trailing-whitespace nil)
  (setq-local header-line-format '(:eval (tterm-header-line-format)))
  (setq-local mode-name '(:eval (tterm--mode-line-mode-name)))
  (setq-local mode-line-misc-info
              (cons '(:eval (tterm--mode-line-input-state))
                    (default-value 'mode-line-misc-info)))
  (setq-local tterm--copy-mode nil)
  (setq-local tterm--cursor-row 0)
  (setq-local tterm--cursor-col 0)
  (setq-local tterm--cursor-visible t)
  (setq-local tterm--cursor-shape 0)
  (setq-local tterm--alt-screen nil)
  (setq-local buffer-read-only t)
  (when (fboundp 'yank-media-handler)
    (yank-media-handler "^image/" #'tterm--yank-media-file))
  (tterm--set-input-mode 'normal)
  (add-hook 'kill-buffer-hook #'tterm--kill-buffer nil t)
  (tterm--install-buffer-hooks))

(defun tterm--disable-font-lock ()
  "Keep font-lock from stripping terminal face text properties."
  (when (eq major-mode 'tterm-mode)
    (font-lock-mode -1)))

(add-hook 'tterm-mode-hook #'tterm--disable-font-lock)

(tterm--refresh-live-buffer-hooks)

(provide 'tterm-mode)
;;; tterm-mode.el ends here
