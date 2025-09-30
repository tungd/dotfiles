;;; codex.el --- Per-project Codex CLI in vterm -*- lexical-binding: t; -*-

;; Minimal integration to run Codex CLI in a per-project vterm buffer.

;;; Commentary:
;;
;; Provides `codex-start' to launch (or switch to) a Codex CLI session
;; scoped to the current project. It uses the vterm terminal emulator so
;; the CLI runs as an interactive TTY and scrollback works well.
;;
;; Customization variables let you pick the program and its args.
;;
;;; Code:

(require 'subr-x)
(require 'project)
(require 'rx)
(require 'thingatpt)
(require 'transient)

(autoload 'which-function "which-func" nil t)

(declare-function vterm-mode "vterm")
(declare-function vterm-send-key "vterm" (key &optional shift meta ctrl accept-proc-output))
(declare-function vterm-send-string "vterm" (string &optional newline))
(declare-function vterm-send-return "vterm")
(defvar vterm-shell)

(defgroup codex nil
  "Run Codex CLI in a per-project vterm buffer."
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

(defcustom codex-max-snippet-length 1200
  "Maximum number of characters from a region to include in prompts."
  :type 'integer
  :group 'codex)

(defcustom codex-send-timeout 5.0
  "Seconds to wait for the Codex prompt before sending automated prompts."
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
map (usually vterm's) and adds:
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
  (if (derived-mode-p 'vterm-mode)
      (vterm-send-key "<up>")
    (message "[Codex] Not a vterm buffer")))

(defun codex-send-down ()
  "Send a Down Arrow keypress to the underlying terminal."
  (interactive)
  (if (derived-mode-p 'vterm-mode)
      (vterm-send-key "<down>")
    (message "[Codex] Not a vterm buffer")))

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
  "Start a Codex CLI vterm session in project P."
  (let* ((buffer (get-buffer-create (codex--buffer-name p)))
         (project-root (when p (project-root p)))
         (args (remove nil codex-args))
         (command (mapconcat #'shell-quote-argument
                             (cons codex-program args)
                             " ")))
    (with-current-buffer buffer
      (unless (require 'vterm nil t)
        (user-error "[Codex] vterm package is required"))
      (when project-root
        (setq default-directory (file-name-as-directory project-root)))
      ;; Make sure any previous poller is cleared before we recreate the buffer
      ;; state.
      (codex--stop-poller)
      ;; Launch vterm with our Codex command.
      (let ((process-adaptive-read-buffering nil)
            (vterm-shell command))
        (vterm-mode))
      (when project-root
        (setq default-directory (file-name-as-directory project-root)))

      ;; Buffer-local scrolling tweaks to keep the viewport stable while Codex
      ;; streams output.
      (setq-local scroll-conservatively 10000)
      (setq-local scroll-margin 0)
      (setq-local maximum-scroll-margin 0)
      (setq-local scroll-preserve-screen-position t)
      (setq-local auto-window-vscroll nil)
      (setq-local scroll-step 1)
      (setq-local hscroll-step 1)
      (setq-local hscroll-margin 0)
      (setq-local line-spacing 0)
      (setq-local vertical-scroll-bar nil)
      (setq-local fringe-mode 0)

      ;; Enable transcript navigation minor mode.
      (codex-transcript-mode 1)
      ;; Local keymap overrides are installed by the minor mode.
      (buffer-name))))

;;
;; Prompt helpers
;;

(defun codex--relative-file-name ()
  "Return buffer file as project-relative path when available."
  (when buffer-file-name
    (let* ((proj (project-current nil))
           (root (and proj (project-root proj))))
      (if (and root (file-in-directory-p buffer-file-name root))
          (file-relative-name buffer-file-name root)
        (abbreviate-file-name buffer-file-name)))))

(defun codex--clip-text (text)
  "Clip TEXT to `codex-max-snippet-length', appending notice when trimmed."
  (let* ((limit (max 0 codex-max-snippet-length))
         (needs-trim (> (length text) limit)))
    (if (or (zerop limit) (not needs-trim))
        text
      (concat (substring text 0 limit)
              "\n... [truncated; selection continues in buffer] ..."))))

(defun codex--code-fence-language ()
  "Return an approximate language tag for the current buffer."
  (string-remove-suffix "-mode" (symbol-name major-mode)))

(defun codex--context-block (context &optional include-region)
  "Build a context block for CONTEXT.
When INCLUDE-REGION is non-nil, include region line numbers."
  (let* ((file (plist-get context :relative-file))
         (func (plist-get context :current-function))
         (start (plist-get context :region-start-line))
         (end   (plist-get context :region-end-line))
         (parts (delq nil
                      (list (when file (format "File: %s" file))
                            (when func (format "Function: %s" func))
                            (when (and include-region (plist-get context :region-active) start end)
                              (format "Selection: lines %d-%d" start end))))))
    (if parts
        (concat "\n\nContext:\n" (string-join parts "\n"))
      "")))

(defun codex--format-text-block (label text &optional language)
  "Return a fenced block with LABEL for TEXT using LANGUAGE."
  (let ((content (and text (string-trim-right text "\n+"))))
    (when (and content (not (string-blank-p content)))
      (format "\n%s:\n```%s\n%s\n```"
              label
              (or language "text")
              content))))

(defun codex--session-buffer ()
  "Return the Codex session buffer for the current project, starting one if needed."
  (let* ((proj (project-current nil))
         (name (codex--buffer-name proj))
         (buffer (get-buffer name)))
    (unless (buffer-live-p buffer)
      (save-window-excursion
        (codex-start))
      (setq buffer (get-buffer name)))
    buffer))

(defun codex--wait-for-ready (buffer &optional timeout)
  "Wait until BUFFER is ready for input or TIMEOUT seconds elapse.
Returns non-nil when the prompt looked idle."
  (with-current-buffer buffer
    (let* ((limit (+ (float-time) (or timeout codex-send-timeout)))
           (interval 0.1))
      (while (and (codex-busy-p)
                  (< (float-time) limit))
        (sit-for interval)))
    (with-current-buffer buffer (not (codex-busy-p)))))

(defun codex--send-prompt (prompt)
  "Send PROMPT to the active Codex CLI session."
  (let ((text (string-trim-right prompt)))
    (when (string-empty-p text)
      (user-error "[Codex] Cannot send an empty prompt"))
    (let ((buffer (codex--session-buffer)))
      (unless (buffer-live-p buffer)
        (user-error "[Codex] Unable to start Codex session"))
      (codex--wait-for-ready buffer codex-send-timeout)
      (display-buffer buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (vterm-send-string text)
        (vterm-send-return))
      (message "[Codex] Sent prompt (%d chars)" (length text)))))

;;
;; Code change prompt
;;

(defun codex--gather-context ()
  "Collect contextual details for the current buffer.
Returns a plist containing file, function, region and defun metadata."
  (let* ((region-active (use-region-p))
         (region-beg (and region-active (region-beginning)))
         (region-end (and region-active (region-end)))
         (defun-bounds (unless region-active (bounds-of-thing-at-point 'defun)))
         (start (or region-beg (car defun-bounds) (line-beginning-position)))
         (end (or region-end (cdr defun-bounds) (line-end-position)))
         (snippet (buffer-substring-no-properties start end))
         (file-name (codex--relative-file-name))
         (function-name (when (fboundp 'which-function) (which-function))))
    (list :region-active region-active
          :region-text (when region-active
                         (buffer-substring-no-properties region-beg region-end))
          :region-start-line (when region-active (line-number-at-pos region-beg))
          :region-end-line (when region-active (line-number-at-pos region-end))
          :defun-start (when defun-bounds (line-number-at-pos (car defun-bounds)))
          :defun-end (when defun-bounds (line-number-at-pos (cdr defun-bounds)))
          :snippet snippet
          :snippet-start (line-number-at-pos start)
          :snippet-end (line-number-at-pos end)
          :relative-file file-name
          :current-function function-name)))

(defun codex--format-location (context)
  "Format a human-readable location string from CONTEXT."
  (let ((file (or (plist-get context :relative-file) "(buffer)"))
        (start (plist-get context :snippet-start))
        (end (plist-get context :snippet-end)))
    (format "%s:%d-%d" file start end)))

;;;###autoload
(defun codex-code-change ()
  "Collect code context and prompt Codex to make a targeted change."
  (interactive)
  (unless buffer-file-name
    (user-error "[Codex] Current buffer is not visiting a file"))
  (let* ((context (codex--gather-context))
         (desc (string-trim (read-from-minibuffer "Describe the change: "))))
    (when (string-empty-p desc)
      (user-error "[Codex] Change description cannot be empty"))
    (let* ((location (codex--format-location context))
           (function-name (plist-get context :current-function))
           (function-line (when function-name
                            (format "\nFunction: %s" function-name)))
           (code-block
            (codex--format-text-block
             "Current code"
             (codex--clip-text (plist-get context :snippet))
             (codex--code-fence-language)))
           (instruction
            (mapconcat #'identity
                       (list desc
                             (format "\nContext: %s" location)
                             (or function-line "")
                             (or code-block "")
                             "\nApply the requested change and explain the modifications you made."
                             "Re-run the relevant automated tests and update them if they fail."
                             "If the request is ambiguous, ask for clarification before proceeding.")
                       "\n")))
      (codex--send-prompt instruction)
      (message "[Codex] Requested code change for %s" location))))

;;
;; Implement TODO prompt
;;

(defun codex--is-comment-line (line)
  "Return non-nil when LINE looks like a comment in the current mode."
  (when comment-start
    (let ((prefix (string-trim-right comment-start)))
      (string-match-p (concat "^[\t ]*" (regexp-quote prefix) "+")
                      (string-trim-left (or line ""))))))

;;;###autoload
(defun codex-implement-todo (arg)
  "Prepare a Codex prompt to implement TODO style comments.
With prefix ARG, add implementations after comments instead of replacing them."
  (interactive "P")
  (unless buffer-file-name
    (user-error "[Codex] Current buffer is not visiting a file"))
  (let* ((context (codex--gather-context))
         (current-line (string-trim (or (thing-at-point 'line t) "")))
         (current-line-number (line-number-at-pos (point)))
         (is-comment (codex--is-comment-line current-line))
         (function-name (plist-get context :current-function))
         (function-context (if function-name (format "\nFunction: %s" function-name) ""))
         (region-active (plist-get context :region-active))
         (region-text (when region-active (codex--clip-text (plist-get context :region-text))))
         (region-start-line (plist-get context :region-start-line))
         (file-name (plist-get context :relative-file))
         (file-context (if file-name (format "\nFile: %s" file-name) ""))
         (initial
          (if arg
              (cond
               (region-text
                (format "Please implement code after this requirement comment block starting on line %d: '%s'. Leave the comment as-is and add the implementation code after it. Keep the existing code structure and add the implementation after this specific block.%s%s"
                        region-start-line region-text function-context file-context))
               (is-comment
                (format "Please implement code after this requirement comment on line %d: '%s'. Leave the comment as-is and add the implementation code after it. Keep the existing code structure and add the implementation after this specific comment.%s%s"
                        current-line-number current-line function-context file-context))
               (function-name
                (format "Please implement code after all TODO comments in function '%s'. The TODO are TODO comments. Leave the comments as-is and add implementation code after each comment. Keep the existing code structure and only add code after these marked items.%s"
                        function-name file-context))
               (t
                (format "Please implement code after all TODO comments in file '%s'. The TODO are TODO comments. Leave the comments as-is and add implementation code after each comment. Keep the existing code structure and only add code after these marked items.%s"
                        (file-name-nondirectory buffer-file-name) file-context)))
            (cond
             (region-text
              (format "Please implement this requirement comment block starting on line %d in-place: '%s'. It is already inside current code. Please replace it with implementation. Keep the existing code structure and implement just this specific block.%s%s"
                      region-start-line region-text function-context file-context))
             (is-comment
              (format "Please implement this requirement comment on line %d in-place: '%s'. It is already inside current code. Please replace it with implementation. Keep the existing code structure and implement just this specific comment.%s%s"
                      current-line-number current-line function-context file-context))
             (function-name
              (format "Please implement all TODO in-place in function '%s'. The TODO are TODO comments. Keep the existing code structure and only implement these marked items.%s"
                      function-name file-context))
             (t
              (format "Please implement all TODO in-place in file '%s'. The TODO are TODO comments. Keep the existing code structure and only implement these marked items.%s"
                      (file-name-nondirectory buffer-file-name) file-context))))
         (prompt (read-from-minibuffer "TODO implementation instruction: " initial)))
    (codex--send-prompt prompt)
    (message "[Codex] Requested TODO implementation")))

;;;###autoload
(defun codex-fix-tests ()
  "Send a prompt instructing Codex to run and fix the project's tests.
The active region is treated as failure context and included in the prompt."
  (interactive)
  (let* ((context (codex--gather-context))
         (failure-snippet (when (plist-get context :region-active)
                            (codex--clip-text (plist-get context :region-text))))
         (failure-block (codex--format-text-block "Failure context" failure-snippet "text"))
         (default (concat "Determine how to run the project's automated test suite from the repository root."
                          "\nRun the tests, capture any failures, diagnose the root causes, apply the minimal code changes needed to fix them, and rerun the tests until they pass."
                          "\nExplain the commands you executed, list failing cases before and after fixes, and summarize the final status."
                          "\nIf the information provided is insufficient, ask clarifying questions before making changes."
                          (codex--context-block context nil)
                          (or failure-block "")
                          "\nMaintain existing behavior outside the failing scenarios and avoid unrelated edits."))
         (prompt (read-from-minibuffer "Fix tests prompt: " default)))
    (codex--send-prompt prompt)
    (message "[Codex] Requested test fix workflow")))

;;
;; Transient menu
;;

;;;###autoload
(transient-define-prefix codex-command-menu ()
  "Transient menu for common Codex CLI prompts."
  [["Codex prompts"
    ["Code"
     ("c" "Code change" codex-code-change)
     ("t" "Implement TODO" codex-implement-todo)
     ("f" "Fix tests" codex-fix-tests)]]])

;;;###autoload
(defun codex-start (&optional restart)
  "Start or switch to the per-project Codex CLI vterm buffer.

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
