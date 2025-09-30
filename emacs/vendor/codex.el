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
(defvar vterm-timer-delay)

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

(defcustom codex-after-response-check-interval 0.5
  "Polling interval in seconds used when waiting for Codex to finish responding."
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

(defcustom codex-after-response-hook nil
  "Hook run in the originating buffer after Codex finishes responding.
Each function should accept no arguments."
  :type 'hook
  :group 'codex)

(defconst codex--expected-last-line
  " ⏎ send   Ctrl+J newline   Ctrl+T transcript   Ctrl+C quit"
  "Substring that, when present in the last line, indicates Codex is waiting for input.")

(defvar-local codex--pending-actions nil
  "Queue of functions to run after Codex finishes producing output.")

(defvar-local codex--after-response-timer nil
  "Timer object used to monitor the Codex buffer for idle state.")

(defvar-local codex--initial-resize-done nil
  "Non-nil once the initial flicker-avoidance resize ran in this buffer.")

(defun codex--maybe-nudge-window (buffer)
  "Work around initial edge flicker by briefly resizing BUFFER's window."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless codex--initial-resize-done
        (setq codex--initial-resize-done t)
        (when-let* ((win (get-buffer-window buffer t)))
          (with-selected-window win
            (condition-case nil
                (progn
                  (enlarge-window-horizontally 1)
                  (shrink-window-horizontally 1))
              (error nil))))))))

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

(defun codex--goto-turn (direction)
  "Move point to the start of the next/previous chat turn.

DIRECTION is 1 for next, -1 for previous. Returns non-nil if movement
occurred, otherwise leaves point and returns nil."
  (let* ((case-fold-search nil)
         (bullet (char-to-string #x2022))
         (regex (concat "^\\s-*\\(?:"
                        (regexp-opt (list bullet ">"))
                        "\\)"))
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
  "Jump to the start of the next chat turn."
  (interactive)
  (unless (codex--goto-turn +1)
    (message "No next chat turn")))

(defun codex-previous-turn ()
  "Jump to the start of the previous chat turn."
  (interactive)
  (unless (codex--goto-turn -1)
    (message "No previous chat turn")))

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
      ;; Reset any pending after-response state before relaunching vterm.
      (when (timerp codex--after-response-timer)
        (cancel-timer codex--after-response-timer))
      (setq codex--after-response-timer nil
            codex--pending-actions nil)
      ;; Launch vterm with our Codex command.
      (let ((vterm-shell command))
        (vterm-mode))
      (when project-root
        (setq default-directory (file-name-as-directory project-root)))

      (setq-local vterm-timer-delay 0.02)

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

(defun codex--send-prompt (prompt &optional after-action)
  "Send PROMPT to the active Codex CLI session."
  (let ((text (string-trim-right prompt)))
    (when (string-empty-p text)
      (user-error "[Codex] Cannot send an empty prompt"))
    (let ((buffer (codex--session-buffer)))
      (unless (buffer-live-p buffer)
        (user-error "[Codex] Unable to start Codex session"))
      (codex--wait-for-ready buffer codex-send-timeout)
      (display-buffer buffer)
      (codex--maybe-nudge-window buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (vterm-send-string text)
        (vterm-send-return)
        (when after-action
          (codex--queue-after-response buffer after-action)))
      (message "[Codex] Sent prompt (%d chars)" (length text)))))

(defun codex--queue-after-response (buffer action)
  "Schedule ACTION to run when Codex BUFFER appears idle."
  (when (functionp action)
    (with-current-buffer buffer
      (setq codex--pending-actions
            (append codex--pending-actions (list action))))
    (codex--ensure-after-response-timer buffer)))

(defun codex--ensure-after-response-timer (buffer)
  "Ensure BUFFER has an active timer watching for Codex idle state."
  (with-current-buffer buffer
    (unless (timerp codex--after-response-timer)
      (setq codex--after-response-timer
            (run-at-time codex-after-response-check-interval nil
                         #'codex--after-response-check buffer)))))

(defun codex--after-response-check (buffer)
  "Process pending after-response actions for BUFFER if Codex is idle."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq codex--after-response-timer nil)
      (cond
       ((null codex--pending-actions)
        ;; Nothing to do.
        nil)
       ((codex-busy-p)
        ;; Still streaming output; re-arm the timer.
        (codex--ensure-after-response-timer buffer))
       (t
        (let ((actions codex--pending-actions))
          (setq codex--pending-actions nil)
          (dolist (fn actions)
            (condition-case err
                (funcall fn)
              (error (message "[Codex] After-response action failed: %s"
                              (error-message-string err)))))
          (when codex--pending-actions
            (codex--ensure-after-response-timer buffer))))))))

(defun codex--compose-after-response (buffer extra-fns)
  "Return a closure that runs hooks in BUFFER followed by EXTRA-FNS.
EXTRA-FNS should be a list of zero-argument functions."
  (when (or codex-after-response-hook extra-fns)
    (lambda ()
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (run-hooks 'codex-after-response-hook)
          (dolist (fn extra-fns)
            (condition-case err
                (funcall fn)
              (error (message "[Codex] After-response hook failed: %s"
                              (error-message-string err))))))))))

(defun codex-after-response-show-magit-diff ()
  "Show the current unstaged diff using Magit, when available."
  (if (fboundp 'magit-diff-unstaged)
      (magit-diff-unstaged)
    (message "[Codex] Magit is not available to display a diff.")))

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

(defconst codex--code-change-default-postamble
  '("Apply the requested change and explain the modifications you made."
    "Re-run the relevant automated tests and update them if they fail."
    "If the request is ambiguous, ask for clarification before proceeding.")
  "Default trailing instructions appended to Codex code-change prompts.")

(defvar-local codex-last-intent nil
  "Symbol describing the most recent Codex intent triggered from this buffer.")

(defun codex--code-change (initial &optional postamble extra-hooks intent)
  "Internal helper to prompt for a code change using INITIAL text.
POSTAMBLE, when non-nil, should be a list of trailing instructions.
EXTRA-HOOKS is a list of functions to run after Codex responds.
INTENT is recorded in `codex-last-intent' for downstream hooks."
  (unless buffer-file-name
    (user-error "[Codex] Current buffer is not visiting a file"))
  (let* ((context (codex--gather-context))
         (desc (string-trim (read-from-minibuffer "Describe the change: " (or initial "")))))
    (when (string-empty-p desc)
      (user-error "[Codex] Change description cannot be empty"))
    (setq codex-last-intent intent)
    (let* ((location (codex--format-location context))
           (function-name (plist-get context :current-function))
           (function-line (when function-name (format "Function: %s" function-name)))
           (code-block
            (codex--format-text-block
             "Current code"
             (codex--clip-text (plist-get context :snippet))
             (codex--code-fence-language)))
           (post-lines (or postamble codex--code-change-default-postamble))
           (post-text (when post-lines (string-join post-lines "\n")))
           (segments (delq nil (list desc
                                     (format "Context: %s" location)
                                     function-line
                                     code-block
                                     post-text)))
           (prompt (string-join segments "\n\n"))
           (after (codex--compose-after-response (current-buffer) extra-hooks)))
      (codex--send-prompt prompt after)
      (message "[Codex] Requested code change for %s" location))))

;;;###autoload
(defun codex-code-change ()
  "Collect code context and prompt Codex to make a targeted change."
  (interactive)
  (codex--code-change nil nil nil 'custom))

(defun codex-code-change-add-logging ()
  "Ask Codex to add structured logging to the current code context."
  (interactive)
  (codex--code-change
   "Add structured logging that captures key inputs, outputs, and exceptional conditions without changing the behavior. Use existing logging utilities if available."
   nil
   'logging))

(defun codex-code-change-refactor-readability ()
  "Ask Codex to refactor the current code for readability."
  (interactive)
  (codex--code-change
   "Refactor this code to improve readability and clarity. Reduce duplication, rename unclear identifiers, and reorganize logic while preserving observable behavior."
   nil
   'refactor-readability))

(defun codex-code-change-simplify ()
  "Ask Codex to simplify the selected code."
  (interactive)
  (codex--code-change
   "Simplify this code by reducing unnecessary branching or complexity while keeping the same behavior and edge-case handling."
   nil
   'simplify))

(defun codex-code-change-add-docs ()
  "Ask Codex to add documentation and comments to the current code."
  (interactive)
  (codex--code-change
   "Add concise documentation and inline comments that clarify the purpose, inputs, outputs, and important invariants of this code. Avoid redundant comments."
   nil
   'add-docs))

(defun codex-code-change-describe ()
  "Ask Codex to describe what the current code does without modifying it."
  (interactive)
  (codex--code-change
   "Describe in detail what this code does, including inputs, outputs, side effects, and tricky edge cases. Highlight any risks or assumptions."
   '("Provide an explanation only; do not modify the code or run commands.")
   nil
   'describe))

(defun codex-code-change-write-tests ()
  "Ask Codex to write or update tests for the current code."
  (interactive)
  (codex--code-change
   "Write or update automated tests that thoroughly cover this behavior, including success, failure, and edge cases."
   '("Focus on creating or updating automated tests for the described behavior."
     "Run the relevant test suite and report the results.")
   nil
   'write-tests))

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
  (setq codex-last-intent 'implement-todo)
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
         (prompt (read-from-minibuffer "TODO implementation instruction: " initial))
         (after (codex--compose-after-response (current-buffer) nil)))
    (codex--send-prompt prompt after)
    (message "[Codex] Requested TODO implementation"))))

;;;###autoload
(defun codex-fix-tests ()
  "Send a prompt instructing Codex to run and fix the project's tests.
The active region is treated as failure context and included in the prompt."
  (interactive)
  (setq codex-last-intent 'fix-tests)
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
         (prompt (read-from-minibuffer "Fix tests prompt: " default))
         (after (codex--compose-after-response (current-buffer) nil)))
    (codex--send-prompt prompt after)
    (message "[Codex] Requested test fix workflow")))

;;
;; Transient menu
;;

;;;###autoload
(transient-define-prefix codex-command-menu ()
  "Transient menu for common Codex CLI prompts."
  [["Code change"
    ["c" "Custom" codex-code-change]
    ["l" "Add logging" codex-code-change-add-logging]
    ["r" "Refactor readability" codex-code-change-refactor-readability]
    ["s" "Simplify" codex-code-change-simplify]
    ["a" "Add docs/comments" codex-code-change-add-docs]
    ["e" "Describe" codex-code-change-describe]
    ["w" "Write tests" codex-code-change-write-tests]]
   ["Other"
    ["t" "Implement TODO" codex-implement-todo]
    ["f" "Fix tests" codex-fix-tests]]])

;;;###autoload
(defun codex-start (&optional restart)
  "Start or switch to the per-project Codex CLI vterm buffer.

With prefix argument RESTART (\[universal-argument]), restart the Codex
process for the current project (kill if running, then start anew)."
  (interactive "P")
  (let* ((proj (project-current))
         (name (codex--buffer-name proj))
         (buf (get-buffer name)))
    (let ((target
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
      (when (buffer-live-p target)
        (codex--maybe-nudge-window target)))))

(provide 'codex)

;;; codex.el ends here
