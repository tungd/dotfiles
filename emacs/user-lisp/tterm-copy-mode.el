;;; tterm-copy-mode.el --- Copy mode for tterm -*- lexical-binding: t; -*-

;; Copy-mode state, scrollback display, and row-offset helpers.

(require 'ansi-color)

(defvar tterm-buffer-size)
(defvar tterm--cursor-col)
(defvar tterm--cursor-row)
(defvar tterm--input-mode)
(defvar tterm--terminal)

(declare-function tterm-id "tterm" (term))
(declare-function tterm-alt-screen "tterm" (term))
(declare-function tterm-rows "tterm" (term))
(declare-function tterm-cols "tterm" (term))
(declare-function tterm--apply-op-data "tterm-apply" (id ops))
(declare-function tterm--blank-lines "tterm-apply" (count))
(declare-function tterm--clear-simple-row-map "tterm" ())
(declare-function tterm--command "tterm" (id command &optional payload))
(declare-function tterm--decode-command-text "tterm" (text))
(declare-function tterm--delete-lines-at "tterm-apply" (row count))
(declare-function tterm--stop-redraw-request-timer "tterm-mode" ())
(declare-function tterm--stop-redraw-timer "tterm-mode" ())
(declare-function tterm--mouse-ignore-down "tterm" (event))
(declare-function tterm--mouse-set-point "tterm" (event))
(declare-function tterm--mouse-set-region "tterm" (event))
(declare-function tterm--set-input-mode "tterm" (mode))
(declare-function tterm-open-hyperlink "tterm-osc" ())
(declare-function tterm-normal-mode "tterm" ())

(defvar-local tterm--copy-mode nil
  "Whether copy mode is active in this buffer.")

(defvar-local tterm--copy-mode-row-offset 0
  "Number of rows that terminal frames should be shifted down by in copy mode.")

(defvar-local tterm--copy-mode-scrollback-rows 0
  "Number of rows currently shown as copy-mode scrollback.")

(defvar-local tterm--copy-mode-history-rows 0
  "Number of tmux history rows fetched into copy mode.")

(defcustom tterm-copy-mode-scrollback-rows 2000
  "How many historical lines to fetch when entering copy mode.
When nil, fetch up to `tterm-buffer-size'."
  :type '(choice (const :tag "Use tterm-buffer-size" nil)
                 integer)
  :group 'tterm)

(defun tterm--row-offset ()
  "Number of rows that terminal drawing should be shifted down."
  (or tterm--copy-mode-row-offset 0))

(defun tterm--set-row-offset (value)
  "Set the terminal drawing row offset to VALUE."
  (setq-local tterm--copy-mode-row-offset (max 0 value)))

(defun tterm--shift-row (row)
  "Shift ROW by the current copy-mode offset."
  (+ row (tterm--row-offset)))

(defun tterm--count-apply-op-rows (op)
  "Return the visible row count represented by apply OP."
  (pcase (aref op 0)
    ('A
     (let ((rows (aref op 1)))
       (cond
        ((vectorp rows) (length rows))
        ((listp rows) (length rows))
        (t 0))))
    ('R (aref op 2))
    (_ 0)))

(defun tterm--count-scrollback-rows (ops)
  "Return the total visible rows contained in scrollback apply OPS."
  (let ((count 0))
    (dolist (op ops count)
      (setq count (+ count (tterm--count-apply-op-rows op))))))

(defun tterm--insert-copy-mode-gap (rows)
  "Insert ROWS blank terminal lines before the visible screen."
  (when (> rows 0)
    (save-excursion
      (goto-char (point-min))
      (insert (tterm--blank-lines rows)))
    (tterm--clear-simple-row-map)))

(defun tterm--restore-copy-mode-display ()
  "Remove copy-mode scrollback rows and clear offset."
  (let ((rows tterm--copy-mode-scrollback-rows))
    (when (> rows 0)
      (condition-case err
          (let ((inhibit-read-only t))
            (tterm--delete-lines-at 0 rows))
        (error
         (message "tterm--restore-copy-mode-display: delete-lines-at failed: %s"
                  (error-message-string err)))))
    (setq-local tterm--copy-mode-scrollback-rows 0)
    (setq-local tterm--copy-mode-history-rows 0)
    (tterm--set-row-offset 0)))

(defun tterm--copy-mode-promotable-scroll-p (top bottom delta)
  "Return non-nil when scroll TOP BOTTOM DELTA adds terminal scrollback."
  (and tterm--copy-mode
       tterm--terminal
       (< delta 0)
       (= top 0)
       (= bottom (tterm-rows tterm--terminal))
       (not (tterm-alt-screen tterm--terminal))))

(defun tterm--copy-mode-promote-scrolled-lines (row count)
  "Duplicate COUNT live rows at ROW into the copy-mode scrollback area."
  (when (and tterm--copy-mode (> count 0))
    (save-excursion
      (goto-char (point-min))
      (forward-line row)
      (let ((start (point)))
        (forward-line count)
        (let ((text (buffer-substring start (point)))
              (inhibit-read-only t)
              (inhibit-modification-hooks t)
              (buffer-undo-list t))
          (goto-char start)
          (insert text))))
    (setq-local tterm--copy-mode-scrollback-rows
                (+ tterm--copy-mode-scrollback-rows count))
    (tterm--set-row-offset (+ (tterm--row-offset) count))
    (tterm--clear-simple-row-map)))

(defun tterm--copy-mode-unescape-field (value)
  "Decode one escaped copy-history field VALUE."
  (let ((index 0)
        (length (length value))
        (out nil))
    (while (< index length)
      (let ((char (aref value index)))
        (if (and (= char ?\\) (< (1+ index) length))
            (let ((next (aref value (1+ index))))
              (push (pcase next
                      (?n ?\n)
                      (?t ?\t)
                      (?\\ ?\\)
                      (_ next))
                    out)
              (setq index (+ index 2)))
          (push char out)
          (setq index (1+ index)))))
    (apply #'string (nreverse out))))

(defun tterm--copy-mode-fit-row (text cols)
  "Return TEXT truncated or padded to COLS cells."
  (let ((value (if (> (length text) cols)
                   (substring text 0 cols)
                 text)))
    (concat value (make-string (max 0 (- cols (length value))) ?\s))))

(defun tterm--copy-mode-bold-face-p (face)
  "Return non-nil when FACE represents ANSI bold."
  (memq face '(bold ansi-color-bold)))

(defun tterm--copy-mode-normalized-face-list (face)
  "Return FACE as a list, with theme-colored bold faces weight-only."
  (cond
   ((null face) nil)
   ((tterm--copy-mode-bold-face-p face) (list '(:weight bold)))
   ((and (consp face) (keywordp (car face))) (list face))
   ((consp face)
    (apply #'append (mapcar #'tterm--copy-mode-normalized-face-list face)))
   (t (list face))))

(defun tterm--copy-mode-normalize-face (face)
  "Normalize ANSI FACE for tterm copy-mode rendering."
  (let ((faces (tterm--copy-mode-normalized-face-list face)))
    (cond
     ((null faces) nil)
     (t faces))))

(defun tterm--copy-mode-promote-font-lock-faces (text)
  "Move ANSI `font-lock-face' properties in TEXT to normalized `face'."
  (let ((pos 0)
        (len (length text)))
    (while (< pos len)
      (let* ((next-font-lock
              (next-single-property-change pos 'font-lock-face text len))
             (next-face
              (next-single-property-change pos 'face text len))
             (next (min next-font-lock next-face))
             (font-lock-face (get-text-property pos 'font-lock-face text))
             (face (or font-lock-face (get-text-property pos 'face text))))
        (when face
          (put-text-property
           pos next 'face (tterm--copy-mode-normalize-face face) text))
        (when font-lock-face
          (remove-text-properties pos next '(font-lock-face nil) text))
        (setq pos next))))
  text)

(defun tterm--copy-mode-strip-osc-controls (text)
  "Remove OSC control strings from tmux history TEXT."
  (let ((len (length text))
        (pos 0)
        (copy-start 0)
        (changed nil)
        chunks)
    (while (< pos len)
      (if (and (= (aref text pos) ?\e)
               (< (1+ pos) len)
               (= (aref text (1+ pos)) ?\]))
          (let ((scan (+ pos 2))
                end)
            (while (and (< scan len) (not end))
              (cond
               ((= (aref text scan) ?\a)
                (setq end (1+ scan)))
               ((and (= (aref text scan) ?\e)
                     (< (1+ scan) len)
                     (= (aref text (1+ scan)) ?\\))
                (setq end (+ scan 2)))
               (t
                (setq scan (1+ scan)))))
            (if end
                (progn
                  (setq changed t)
                  (when (> pos copy-start)
                    (push (substring text copy-start pos) chunks))
                  (setq pos end)
                  (setq copy-start pos))
              (setq pos (1+ pos))))
        (setq pos (1+ pos))))
    (if changed
        (progn
          (when (< copy-start len)
            (push (substring text copy-start) chunks))
          (apply #'concat (nreverse chunks)))
      text)))

(defun tterm--copy-mode-decode-history-row (text cols)
  "Decode tmux captured history TEXT and fit it to COLS cells."
  (let ((text (tterm--copy-mode-strip-osc-controls text)))
    (tterm--copy-mode-fit-row
     (if (string-match-p "\e" text)
         (tterm--copy-mode-promote-font-lock-faces (ansi-color-apply text))
       text)
     cols)))

(defun tterm--decode-copy-history-ops (text cols)
  "Decode copy-history TEXT into plain row apply ops for COLS."
  (let ((text (tterm--decode-command-text text))
        rows)
    (dolist (line (split-string text "\n" t))
      (pcase (mapcar #'tterm--copy-mode-unescape-field
                     (split-string line "\t"))
        (`("A" ,row ,value)
         (push (vector (string-to-number row)
                       0
                       cols
                       (tterm--copy-mode-decode-history-row value cols))
               rows))))
    (when rows
      (list (vector 'A (vconcat (nreverse rows)))))))

(defun tterm--copy-mode-history-ops (id count cols &optional offset)
  "Return apply ops for tmux history for terminal ID."
  (let ((text (tterm--command id "copy-history"
                              (format "%d:%d:%d" (or offset 0) count cols))))
    (tterm--decode-copy-history-ops text cols)))

(defun tterm--copy-mode-insert-history-ops (ops)
  "Prepend tmux history OPS to the copy-mode display."
  (when ops
    (let ((scrollback-rows (tterm--count-scrollback-rows ops)))
      (when (> scrollback-rows 0)
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t)
              (buffer-undo-list t))
          (tterm--insert-copy-mode-gap scrollback-rows)
          (let ((tterm--copy-mode-row-offset 0))
            (tterm--apply-op-data (tterm-id tterm--terminal) ops))
          (setq-local tterm--copy-mode-scrollback-rows
                      (+ tterm--copy-mode-scrollback-rows scrollback-rows))
          (setq-local tterm--copy-mode-history-rows
                      (+ tterm--copy-mode-history-rows scrollback-rows))
          (tterm--set-row-offset (+ (tterm--row-offset)
                                    scrollback-rows)))))))

(defun tterm--enter-copy-mode-with-scrollback ()
  "Fetch and display scrollback rows above the visible terminal."
  (let* ((term tterm--terminal)
         (count (max 0 (or tterm-copy-mode-scrollback-rows
                           tterm-buffer-size))))
    (when term
      (let ((ops (if (> count 0)
                     (tterm--copy-mode-history-ops
                      (tterm-id term) count (tterm-cols term))
                   nil)))
        (tterm--copy-mode-insert-history-ops ops)))))

(defun tterm-copy-mode-fetch-older-history ()
  "Fetch the next older tmux history window and prepend it."
  (interactive)
  (unless (and tterm--copy-mode tterm--terminal)
    (user-error "Not in copy mode"))
  (let ((count (max 0 (or tterm-copy-mode-scrollback-rows
                          tterm-buffer-size))))
    (when (> count 0)
      (tterm--copy-mode-insert-history-ops
       (tterm--copy-mode-history-ops
        (tterm-id tterm--terminal)
        count
        (tterm-cols tterm--terminal)
        tterm--copy-mode-history-rows)))))

(defun tterm-copy-mode-scroll-down-command (&optional arg)
  "Scroll down in copy mode, fetching older tmux history at the top."
  (interactive "P")
  (if (bobp)
      (tterm-copy-mode-fetch-older-history)
    (scroll-down-command arg)))

(defun tterm--copy-mode-live-position (&optional row col)
  "Return buffer position for live terminal ROW and COL in copy-mode display."
  (save-excursion
    (goto-char (point-min))
    (forward-line (+ tterm--copy-mode-scrollback-rows (or row 0)))
    (move-to-column (or col 0))
    (point)))

(defun tterm--show-copy-mode-live-screen (&optional cursor-row cursor-col)
  "Keep the live terminal screen visible after adding copy-mode scrollback."
  (let ((live-start (tterm--copy-mode-live-position 0 0))
        (live-point (tterm--copy-mode-live-position cursor-row cursor-col)))
    (goto-char live-point)
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (when (window-live-p window)
        (set-window-start window live-start)
        (set-window-point window live-point)))))

(defun tterm-copy-mode-keyboard-quit ()
  "Deselect the active region, or leave copy mode for normal mode."
  (interactive)
  (unless (eq major-mode 'tterm-mode)
    (user-error "Not in a tterm buffer"))
  (if (use-region-p)
      (deactivate-mark)
    (tterm-normal-mode)))

(defun tterm--copy-mode-trim-line-padding (text)
  "Trim terminal padding whitespace before line ends in TEXT."
  (replace-regexp-in-string "[ \t]+\\(\n\\|\\'\\)" "\\1" text nil nil))

(defun tterm-copy-mode-kill-ring-save ()
  "Copy the active region, then deselect it without editing the terminal."
  (interactive)
  (unless (eq major-mode 'tterm-mode)
    (user-error "Not in a tterm buffer"))
  (unless tterm--copy-mode
    (user-error "Not in copy mode"))
  (unless (use-region-p)
    (user-error "The mark is not active now"))
  (kill-new
   (tterm--copy-mode-trim-line-padding
    (buffer-substring-no-properties (region-beginning) (region-end))))
  (deactivate-mark))

(defun tterm-copy-mode-end-of-visible-line ()
  "Move to the end of visible text on the current terminal line.
Trailing terminal padding spaces are skipped."
  (interactive)
  (let ((start (line-beginning-position))
        (pos (line-end-position)))
    (while (and (> pos start)
                (= (char-before pos) ?\s))
      (setq pos (1- pos)))
    (goto-char pos)))

(defun tterm-copy-mode ()
  "Enter copy mode on the visible terminal display."
  (interactive)
  (unless (eq major-mode 'tterm-mode)
    (user-error "Not in a tterm buffer"))
  (when tterm--copy-mode
    (user-error "Already in copy mode"))
  (when (not tterm--terminal)
    (user-error "No terminal attached"))
  (tterm--stop-redraw-timer)
  (tterm--stop-redraw-request-timer)
  (setq-local tterm--copy-mode t)
  (setq-local tterm--copy-mode-scrollback-rows 0)
  (tterm--set-row-offset 0)
  (let ((cursor-row tterm--cursor-row)
        (cursor-col tterm--cursor-col))
    (tterm--enter-copy-mode-with-scrollback)
    (when (> tterm--copy-mode-scrollback-rows 0)
      (tterm--show-copy-mode-live-screen cursor-row cursor-col)))
  (setq-local buffer-read-only t)
  (tterm--set-input-mode (or tterm--input-mode 'normal))
  (force-mode-line-update))

(defun tterm-copy-mode-exit ()
  "Exit copy mode and restore terminal input."
  (interactive)
  (unless (eq major-mode 'tterm-mode)
    (user-error "Not in a tterm buffer"))
  (when (not tterm--copy-mode)
    (user-error "Not in copy mode"))
  (tterm-normal-mode))

(defvar tterm-copy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'tterm-copy-mode-exit)
    (define-key map (kbd "C-g") #'tterm-copy-mode-keyboard-quit)
    (define-key map (kbd "C-^") #'tterm-copy-mode-exit)
    (define-key map (kbd "C-s") #'isearch-forward)
    (define-key map (kbd "C-r") #'isearch-backward)
    ;; Keep navigation in copy mode, including line and page motion.
    (define-key map (kbd "<up>") #'previous-line)
    (define-key map (kbd "<down>") #'next-line)
    (define-key map (kbd "<left>") #'backward-char)
    (define-key map (kbd "<right>") #'forward-char)
    (define-key map (kbd "<home>") #'move-beginning-of-line)
    (define-key map (kbd "<end>") #'move-end-of-line)
    (define-key map (kbd "C-e") #'tterm-copy-mode-end-of-visible-line)
    (define-key map (kbd "<prior>") #'tterm-copy-mode-scroll-down-command)
    (define-key map (kbd "<next>") #'scroll-up-command)
    ;; Mark/copy actions without sending terminal input.
    (define-key map [down-mouse-1] #'tterm--mouse-ignore-down)
    (define-key map [mouse-1] #'tterm--mouse-set-point)
    (define-key map [drag-mouse-1] #'tterm--mouse-set-region)
    (define-key map (kbd "C-c C-o") #'tterm-open-hyperlink)
    (define-key map (kbd "C-SPC") #'set-mark-command)
    (define-key map [remap kill-region] #'tterm-copy-mode-kill-ring-save)
    (define-key map [remap kill-ring-save] #'tterm-copy-mode-kill-ring-save)
    (define-key map (kbd "M-w") #'tterm-copy-mode-kill-ring-save)
    map)
  "Keymap for copy mode in `tterm-mode'.")

;; Keep reloads from leaving existing sessions on stale global editing bindings.
(define-key tterm-copy-mode-map [down-mouse-1] #'tterm--mouse-ignore-down)
(define-key tterm-copy-mode-map [mouse-1] #'tterm--mouse-set-point)
(define-key tterm-copy-mode-map [drag-mouse-1] #'tterm--mouse-set-region)
(define-key tterm-copy-mode-map [remap kill-region] #'tterm-copy-mode-kill-ring-save)
(define-key tterm-copy-mode-map [remap kill-ring-save] #'tterm-copy-mode-kill-ring-save)
(define-key tterm-copy-mode-map (kbd "M-w") #'tterm-copy-mode-kill-ring-save)

(provide 'tterm-copy-mode)
;;; tterm-copy-mode.el ends here
