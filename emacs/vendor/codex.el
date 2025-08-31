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

(defcustom codex-prompt-regexp nil
  "Regexp that matches the Codex CLI prompt when it is waiting for input.

Set this to your Codex CLI prompt string. When the last line in the
Codex terminal buffer matches this regexp, `codex-waiting-hook' runs.
If nil, waiting detection is disabled."
  :type '(choice (const :tag "Disabled" nil)
                 regexp)
  :group 'codex)

(defcustom codex-waiting-regexps nil
  "List of additional regexps that indicate the CLI is ready for input.

These are matched against the last few lines of terminal output. Useful
if the CLI prints a status line rather than a traditional prompt."
  :type '(repeat regexp)
  :group 'codex)

(defcustom codex-waiting-tail-lines 6
  "Number of lines from the end of the buffer to inspect for waiting regexps."
  :type 'integer
  :group 'codex)

(defvar codex-waiting-hook nil
  "Hook run when a Codex CLI session transitions to a waiting-for-input state.

Functions run with current-buffer set to the Codex terminal buffer.
Only runs on transitions (not on every prompt redraw).")

(defvar-local codex--waiting nil)

(defun codex--buffer-tail (nlines)
  "Return the last NLINES of current buffer as a string."
  (save-excursion
    (goto-char (point-max))
    (forward-line (- nlines))
    (buffer-substring-no-properties (point) (point-max))))

(defun codex--at-waiting-p ()
  "Return non-nil if the session appears to be waiting for input."
  (let ((tail (codex--buffer-tail codex-waiting-tail-lines))
        (last-line (save-excursion
                     (goto-char (point-max))
                     (buffer-substring-no-properties (line-beginning-position)
                                                     (point-max)))))
    (or (and codex-prompt-regexp (string-match-p codex-prompt-regexp last-line))
        (and codex-waiting-regexps
             (seq-some (lambda (re) (string-match-p re tail)) codex-waiting-regexps)))))

(defun codex--after-change (_beg _end _len)
  "Buffer-local change hook to detect Codex prompt transitions."
  (when (and codex-prompt-regexp (eq (current-buffer) (current-buffer)))
    (let ((now (codex--at-waiting-p)))
      (when (and now (not codex--waiting))
        (setq codex--waiting t)
        (run-hooks 'codex-waiting-hook))
      (when (and (not now) codex--waiting)
        (setq codex--waiting nil)))))

(defun codex--install-watchers ()
  "Install buffer-local watchers for waiting-state detection."
  (setq codex--waiting nil)
  (add-hook 'after-change-functions #'codex--after-change nil t)
  (add-hook 'kill-buffer-hook (lambda ()
                                (remove-hook 'after-change-functions #'codex--after-change t))
            nil t))

;;; Helper commands for configuring detection

(defun codex-detect-dump-tail (&optional n)
  "Display the last N lines (default `codex-waiting-tail-lines') for debugging."
  (interactive "P")
  (let ((nlines (or (and (numberp n) n)
                    codex-waiting-tail-lines)))
    (message "%s" (codex--buffer-tail nlines))))

(defun codex-set-waiting-regexp-from-tail ()
  "Set `codex-prompt-regexp' to regexp-quoted last line of buffer."
  (interactive)
  (let* ((last (save-excursion
                 (goto-char (point-max))
                 (string-trim-right
                  (buffer-substring-no-properties (line-beginning-position)
                                                  (point-max)))))
         (re (concat "^" (regexp-quote last) "$")))
    (setq codex-prompt-regexp re)
    (message "codex-prompt-regexp set to %S" re)))

(defun codex-add-waiting-regexp (re)
  "Add RE to `codex-waiting-regexps'."
  (interactive "sAdd waiting regexp: ")
  (add-to-list 'codex-waiting-regexps re)
  (message "Added waiting regexp: %S" re))

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
