;;; codex.el --- Per-project Codex CLI in Eat -*- lexical-binding: t; -*-

;; Minimal integration to run Codex CLI in a per-project Eat buffer.

;;; Commentary:
;;
;; Provides `codex-start' to launch (or switch to) a Codex CLI session
;; scoped to the current project. It uses the Eat terminal emulator so
;; the CLI runs as an interactive TTY and scrollback works well.
;;
;; Customization variables let you pick the program and its args.
;;
;;; Code:

(require 'subr-x)
(require 'project)
(require 'rx)

(defgroup codex nil
  "Run Codex CLI in a per-project Eat buffer."
  :group 'tools
  :prefix "codex-")

(defcustom codex-program "codex"
  "Program name or absolute path for the Codex CLI."
  :type 'string
  :group 'codex)

(defcustom codex-args nil
  "List of extra arguments passed to the Codex CLI."
  :type '(repeat string)
  :group 'codex)

(defcustom codex-buffer-name-format "*codex:%s*"
  "Format string for buffer name. Receives project name."
  :type 'string
  :group 'codex)

(defcustom codex-poll-interval 0.8
  "Polling interval in seconds to check if Codex is waiting for input."
  :type 'number
  :group 'codex)

(defvar codex-waiting-hook nil
  "Hook run when a Codex CLI session transitions to a waiting-for-input state.

Functions run with current-buffer set to the Codex terminal buffer.
Only runs on transitions (not on every prompt redraw).")

(defconst codex--expected-last-line
  " ⏎ send   Ctrl+J newline   Ctrl+T transcript   Ctrl+C quit"
  "Substring that, when present in the last line, indicates Codex is waiting for input.")

(defvar-local codex--waiting nil)
(defvar-local codex--poll-timer nil)

;;
;; Transcript navigation
;;

(defvar codex-transcript-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") #'codex-next-turn)
    (define-key map (kbd "M-p") #'codex-previous-turn)
    map)
  "Keymap for `codex-transcript-mode'.")

(defvar-local codex--saved-local-map nil)
(defvar-local codex--local-map nil)

(define-minor-mode codex-transcript-mode
  "Minor mode for navigating Codex chat transcripts.

Installs a buffer-local keymap that inherits from the current local
map (usually Eat's) and adds:
- M-n/M-p to jump between chat turns
- C-n/C-p to send Down/Up arrow to the terminal"
  :init-value nil
  :lighter " CodexNav"
  :keymap codex-transcript-mode-map
  :group 'codex
  (if codex-transcript-mode
      (progn
        (setq codex--saved-local-map (current-local-map))
        (setq codex--local-map (make-sparse-keymap))
        (set-keymap-parent codex--local-map codex--saved-local-map)
        (define-key codex--local-map (kbd "M-n") #'codex-next-turn)
        (define-key codex--local-map (kbd "M-p") #'codex-previous-turn)
        (define-key codex--local-map (kbd "C-n") #'codex-send-down)
        (define-key codex--local-map (kbd "C-p") #'codex-send-up)
        (use-local-map codex--local-map))
    (when codex--saved-local-map (use-local-map codex--saved-local-map))
    (setq codex--local-map nil
          codex--saved-local-map nil)))

(defun codex-send-up ()
  "Send an Up Arrow keypress to the underlying terminal."
  (interactive)
  (if (bound-and-true-p eat-terminal)
      (eat-term-send-string eat-terminal "\e[A")
    (message "[Codex] Not an Eat buffer")))

(defun codex-send-down ()
  "Send a Down Arrow keypress to the underlying terminal."
  (interactive)
  (if (bound-and-true-p eat-terminal)
      (eat-term-send-string eat-terminal "\e[B")
    (message "[Codex] Not an Eat buffer")))

(defun codex--goto-turn (direction &optional type)
  "Move point to the start of the next/previous chat turn content.

DIRECTION is 1 for next, -1 for previous. If TYPE is 'user or 'codex,
restrict movement to that turn type. Returns non-nil if movement
occurred, otherwise leaves point and returns nil."
  (let* ((case-fold-search nil)
         (regex (pcase type
                  ('user (rx line-start "user" line-end))
                  ('codex (rx line-start "codex" line-end))
                  (_ (rx line-start (or "user" "codex") line-end))))
         (start (point)))
    (cond
     ((> direction 0)
      (end-of-line)
      (when (re-search-forward regex nil t)
        (beginning-of-line)
        t))
     ((< direction 0)
      (beginning-of-line)
      (unless (bobp) (backward-char 1))
      (when (re-search-backward regex nil t)
        (beginning-of-line)
        t))
     (t nil))))

(defun codex-next-turn ()
  "Jump to the start of the next chat turn (user or codex)."
  (interactive)
  (unless (codex--goto-turn +1)
    (message "No next chat turn")))

(defun codex-previous-turn ()
  "Jump to the start of the previous chat turn (user or codex)."
  (interactive)
  (unless (codex--goto-turn -1)
    (message "No previous chat turn")))

(defun codex-next-user ()
  "Jump to the start of the next user prompt."
  (interactive)
  (unless (codex--goto-turn +1 'user)
    (message "No next user prompt")))

(defun codex-previous-user ()
  "Jump to the start of the previous user prompt."
  (interactive)
  (unless (codex--goto-turn -1 'user)
    (message "No previous user prompt")))

(defun codex-next-codex ()
  "Jump to the start of the next Codex response."
  (interactive)
  (unless (codex--goto-turn +1 'codex)
    (message "No next Codex response")))

(defun codex-previous-codex ()
  "Jump to the start of the previous Codex response."
  (interactive)
  (unless (codex--goto-turn -1 'codex)
    (message "No previous Codex response")))

(defun codex--read-last-line ()
  "Return the last line of the current buffer as a string."
  (save-excursion
    (goto-char (point-max))
    (buffer-substring-no-properties (line-beginning-position) (point-max))))

(defun codex--read-tail-lines (n)
  "Return the last N lines of the current buffer as a single string."
  (save-excursion
    (goto-char (point-max))
    (forward-line (- n))
    (buffer-substring-no-properties (line-beginning-position) (point-max))))

(defun codex-busy-p ()
  "Return non-nil if Codex CLI appears busy.

This encapsulates the heuristic by reading the last 5 lines of the
current buffer and the last line prompt. It returns nil only when the
buffer appears to be at the Codex prompt and no in-progress indicator
is detected. In all other ambiguous states, it returns non-nil.

Busy conditions include:
- An in-progress indicator (e.g. \"esc to interrupt\").
- A permission prompt for running a local command (detected by the line
  containing \"Allow command?\")."
  (let* ((last (codex--read-last-line))
         (tail (codex--read-tail-lines 5))
          (ready-line (string-match-p (regexp-quote codex--expected-last-line) last))
         (in-progress (string-match-p "esc to interrupt" (downcase tail)))
         (permission (string-match-p "allow command\\\?" (downcase tail))))
    (cond
     (in-progress t)
     (permission t)
     (ready-line nil)
     (t t))))

(defun codex--poll-waiting-state ()
  "Poll the buffer to detect transition into waiting-for-input state."
  (when (buffer-live-p (current-buffer))
    (let* ((busy (codex-busy-p))
           (now (not busy)))
      (when (and now (not codex--waiting))
        (setq codex--waiting t)
        (run-hooks 'codex-waiting-hook))
      (when (and (not now) codex--waiting)
        (setq codex--waiting nil)))))

(defun codex--start-poller ()
  "Start the periodic polling timer for the current buffer."
  (setq codex--waiting nil)
  (when (timerp codex--poll-timer)
    (cancel-timer codex--poll-timer)
    (setq codex--poll-timer nil))
  (setq codex--poll-timer
        (run-with-timer codex-poll-interval codex-poll-interval
                         (lambda (buf)
                           (when (buffer-live-p buf)
                             (with-current-buffer buf
                               (codex--poll-waiting-state))))
                         (current-buffer)))
  (add-hook 'kill-buffer-hook #'codex--stop-poller nil t))

(defun codex--stop-poller ()
  "Stop and clear the polling timer for the current buffer."
  (when (timerp codex--poll-timer)
    (cancel-timer codex--poll-timer)
    (setq codex--poll-timer nil)))

;;; Helper commands (intentionally minimal with timer-based detection)

(defun codex--buffer-name (project)
  "Compute the Codex buffer name for PROJECT using `project-name'."
  (format codex-buffer-name-format (or (and project (project-name project)) "default")))

(defun codex--start-process (p)
  "Start a Codex CLI Eat session in project P."
  (with-current-buffer (get-buffer-create (codex--buffer-name p))
    (cd (project-root p))
    (let ((process-adaptive-read-buffering nil) ; ??? not sure about this
          (args (remove nil codex-args)))
      (apply #'eat-make (substring (buffer-name) 1 -1) codex-program nil args))

    (setq-local scroll-conservatively 10000)  ; Never recenter
    (setq-local scroll-margin 0)              ; No margin so text goes to edge
    (setq-local maximum-scroll-margin 0)      ; No maximum margin
    (setq-local scroll-preserve-screen-position t)  ; Preserve position during scrolling

    ;; Additional stabilization for blinking character height changes
    (setq-local auto-window-vscroll nil)      ; Disable automatic scrolling adjustments
    (setq-local scroll-step 1)                ; Scroll one line at a time
    (setq-local hscroll-step 1)               ; Horizontal scroll one column at a time
    (setq-local hscroll-margin 0)             ; No horizontal scroll margin

    ;; Force consistent line spacing to prevent height fluctuations
    (setq-local line-spacing 0)               ; No extra line spacing)

    ;; Disable eat's text blinking to reduce display changes
    (when (bound-and-true-p eat-enable-blinking-text)
      (setq-local eat-enable-blinking-text nil))

    ;; Force consistent character metrics for blinking symbols
    ;;(setq-local char-width-table nil)         ; causes emacs to crash!
    (setq-local vertical-scroll-bar nil)      ; Disable scroll bar
    (setq-local fringe-mode 0)                ; Disable fringes that can cause reflow
    ;; Start a simple periodic poller to detect when Codex is waiting.
    (codex--start-poller)
    ;; Enable transcript navigation minor mode
    (codex-transcript-mode 1)
    ;; Local keymap overrides are installed by the minor mode
    (buffer-name)))

;;;###autoload
(defun codex-start (&optional restart)
  "Start or switch to the per-project Codex CLI Eat buffer.

With prefix argument RESTART (\[universal-argument]), restart the Codex
process for the current project (kill if running, then start anew)."
  (interactive "P")
  (let* ((proj (project-current))
         (name (codex--buffer-name proj))
         (buf (get-buffer name)))
    (cond
     ;; Restart requested: kill and recreate.
     (restart
      (when (buffer-live-p buf)
        (when-let* ((proc (get-buffer-process buf)))
          (ignore-errors (kill-process proc)))
        (kill-buffer buf))
      (pop-to-buffer-same-window (codex--start-process proj)))
     ;; Buffer exists: just switch to it.
     ((buffer-live-p buf)
      (pop-to-buffer-same-window buf))
     ;; Otherwise, create a new one.
     (t
      (pop-to-buffer-same-window (codex--start-process proj))))))

(provide 'codex)

;;; codex.el ends here
