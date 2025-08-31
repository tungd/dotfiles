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

(defun codex--read-last-line ()
  "Return the last line of the current buffer as a string."
  (save-excursion
    (goto-char (point-max))
    (buffer-substring-no-properties (line-beginning-position) (point-max))))

(defun codex--poll-waiting-state ()
  "Poll the buffer to detect transition into waiting-for-input state."
  (when (buffer-live-p (current-buffer))
    (let* ((last (codex--read-last-line))
           (now (string-match-p (regexp-quote codex--expected-last-line) last)))
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
