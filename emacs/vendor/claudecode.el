;;; claudecode.el --- Per-project Claude Code CLI in eat -*- lexical-binding: t; -*-

;; Minimal integration to run Claude Code CLI in a per-project eat buffer.

;;; Commentary:
;;
;; Provides `claudecode-start' to launch (or switch to) a Claude Code CLI session
;; scoped to the current project. It uses the eat terminal emulator so
;; the CLI runs as an interactive TTY.
;;
;; Customization variables let you pick the program and its args.
;;
;;; Code:

(require 'subr-x)
(require 'project)
(require 'thingatpt)
(require 'transient)

(autoload 'which-function "which-func" nil t)

(declare-function eat "eat")
(declare-function eat-make "eat")
(declare-function eat-term-send-string "eat")
(declare-function eat-self-input "eat")
(declare-function eat-char-mode "eat")

(defgroup claudecode nil
  "Run Claude Code CLI in a per-project eat buffer."
  :group 'tools
  :prefix "claudecode-")

(defcustom claudecode-program "claude"
  "Program name or absolute path for the Claude Code CLI."
  :type 'string
  :group 'claudecode)

(defcustom claudecode-args nil
  "List of extra arguments passed to the Claude Code CLI."
  :type '(repeat string)
  :group 'claudecode)

(defcustom claudecode-buffer-name-format "*claude:%s*"
  "Format string for buffer name. Receives project name."
  :type 'string
  :group 'claudecode)

(defcustom claudecode-max-snippet-length 1200
  "Maximum number of characters from a region to include in prompts."
  :type 'integer
  :group 'claudecode)

;;
;; Transcript navigation
;;

(defvar claudecode-transcript-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") #'claudecode-next-turn)
    (define-key map (kbd "M-p") #'claudecode-previous-turn)
    map)
  "Keymap for `claudecode-transcript-mode'.")

(defvar-local claudecode--saved-local-map nil)
(defvar-local claudecode--local-map nil)

(define-minor-mode claudecode-transcript-mode
  "Minor mode for navigating Claude Code chat transcripts.

Installs a buffer-local keymap that inherits from the current local
map (usually eat's) and adds:
- M-n/M-p to jump between chat turns"
  :init-value nil
  :lighter " ClaudeNav"
  :keymap claudecode-transcript-mode-map
  :group 'claudecode
  (if claudecode-transcript-mode
      (progn
        (setq claudecode--saved-local-map (current-local-map))
        (setq claudecode--local-map (make-sparse-keymap))
        (set-keymap-parent claudecode--local-map claudecode--saved-local-map)
        (define-key claudecode--local-map (kbd "M-n") #'claudecode-next-turn)
        (define-key claudecode--local-map (kbd "M-p") #'claudecode-previous-turn)
        (use-local-map claudecode--local-map))
    (when claudecode--saved-local-map (use-local-map claudecode--saved-local-map))
    (setq claudecode--local-map nil
          claudecode--saved-local-map nil)))

(defun claudecode--goto-turn (direction)
  "Move point to the start of the next/previous chat turn.

DIRECTION is 1 for next, -1 for previous. Returns non-nil if movement
occurred, otherwise leaves point and returns nil."
  (let* ((case-fold-search nil)
         (bullet (char-to-string #x2022))
         (regex (concat "^\\s-*\\(?:"
                        (regexp-opt (list bullet ">" "╭─" "│"))
                        "\\)")))
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

(defun claudecode-next-turn ()
  "Jump to the start of the next chat turn."
  (interactive)
  (unless (claudecode--goto-turn +1)
    (message "No next chat turn")))

(defun claudecode-previous-turn ()
  "Jump to the start of the previous chat turn."
  (interactive)
  (unless (claudecode--goto-turn -1)
    (message "No previous chat turn")))

(defun claudecode-unstick-terminal ()
  "Force redisplay to fix visual artifacts or stuck display.
Use this when the terminal display becomes corrupted."
  (interactive)
  (when-let* ((terminal (bound-and-true-p eat-terminal)))
    ;; Reset terminal size to trigger redraw
    (when (fboundp 'eat-term-resize)
      (let ((width (window-body-width))
            (height (window-body-height)))
        (eat-term-resize terminal width height))))
  (redisplay t)
  (message "[Claude] Terminal display reset"))

;;; Helper commands

(defun claudecode--buffer-name (project)
  "Compute the Claude buffer name for PROJECT using `project-name'."
  (format claudecode-buffer-name-format (or (and project (project-name project)) "default")))

(defun claudecode--start-process (p)
  "Start a Claude Code CLI eat session in project P."
  (let* ((project-root (when p (project-root p)))
         (args (remove nil claudecode-args))
         (buffer-name (claudecode--buffer-name p)))
    (unless (require 'eat nil t)
      (user-error "[Claude] eat package is required"))
    (with-current-buffer (get-buffer-create buffer-name)
      (when project-root
        (cd project-root))
      (apply #'eat-make
             (substring buffer-name 1 -1)  ; strip asterisks for process name
             claudecode-program
             nil
             args)
      ;; Anti-flickering optimizations (from claudemacs)
      (claudecode--setup-buffer-display)
      (claudecode-transcript-mode 1))
    buffer-name))

(defun claudecode--setup-buffer-display ()
  "Configure buffer-local settings to minimize flickering and scroll issues."
  ;; Prevent terminal redraw/scroll reset on buffer switching (critical fix)
  (setq-local window-adjust-process-window-size-function #'ignore)
  ;; Scroll settings to prevent recentering
  (setq-local scroll-conservatively 10000)
  (setq-local scroll-margin 0)
  (setq-local maximum-scroll-margin 0)
  ;; (setq-local scroll-preserve-screen-position t)
  (setq-local auto-window-vscroll nil)
  (setq-local scroll-step 1)
  (setq-local hscroll-step 1)
  (setq-local hscroll-margin 0)
  ;; Display settings
  ;; (setq-local line-spacing 0)
  (setq-local vertical-scroll-bar nil)
  (setq-local fringe-mode 0)
  ;; Disable blinking text (causes constant redraws)
  (when (boundp 'eat-enable-blinking-text)
    (setq-local eat-enable-blinking-text nil))

  ;; === Performance optimizations based on profiling ===

  ;; 1. Mode-line optimization (addresses 60% of redisplay cost)
  ;; Use minimal mode-line to avoid mode-line--minor-modes, mode-line-eol-desc,
  ;; and mode-line-default-help-echo overhead
  (setq-local mode-line-format
              '(" " mode-line-buffer-identification
                " " mode-line-misc-info))

  ;; 2. Disable cursor blinking (reduces redisplay triggers)
  (setq-local cursor-type 'box)
  (when (bound-and-true-p blink-cursor-mode)
    (setq-local blink-cursor-mode nil))

  ;; 3. Disable shell prompt overlay corrections (eat timer overhead)
  ;; Cancel the timer that runs eat--correct-shell-prompt-mark-overlays
  (when (boundp 'eat--shell-prompt-mark-overlays)
    (setq-local eat--shell-prompt-mark-overlays nil))
  ;; Cancel eat's prompt correction timer if it exists
  (when (and (boundp 'eat--shell-prompt-mark-correction-timer)
             (timerp eat--shell-prompt-mark-correction-timer))
    (cancel-timer eat--shell-prompt-mark-correction-timer))
  ;; Disable shell integration which drives these features
  (when (boundp 'eat--shell-integration-enabled)
    (setq-local eat--shell-integration-enabled nil))

  ;; 4. Font cache optimization
  (setq-local inhibit-compacting-font-caches t)

  ;; 5. Reduce redisplay frequency
  (setq-local redisplay-skip-fontification-on-input t)
  (setq-local fast-but-imprecise-scrolling t)

  ;; Use char-mode for more responsive input (bypasses Emacs key processing)
  (when (fboundp 'eat-char-mode)
    (eat-char-mode)))

;;
;; Prompt helpers
;;

(defun claudecode--relative-file-name ()
  "Return buffer file as project-relative path when available."
  (when buffer-file-name
    (let* ((proj (project-current nil))
           (root (and proj (project-root proj))))
      (if (and root (file-in-directory-p buffer-file-name root))
          (file-relative-name buffer-file-name root)
        (abbreviate-file-name buffer-file-name)))))

(defun claudecode--clip-text (text)
  "Clip TEXT to `claudecode-max-snippet-length', appending notice when trimmed."
  (let* ((limit (max 0 claudecode-max-snippet-length))
         (needs-trim (> (length text) limit)))
    (if (or (zerop limit) (not needs-trim))
        text
      (concat (substring text 0 limit)
              "\n... [truncated; selection continues in buffer] ..."))))

(defun claudecode--code-fence-language ()
  "Return an approximate language tag for the current buffer."
  (string-remove-suffix "-mode" (symbol-name major-mode)))

(defun claudecode--context-block (context &optional include-region)
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

(defun claudecode--format-text-block (label text &optional language)
  "Return a fenced block with LABEL for TEXT using LANGUAGE."
  (let ((content (and text (string-trim-right text "\n+"))))
    (when (and content (not (string-blank-p content)))
      (format "\n%s:\n```%s\n%s\n```"
              label
              (or language "text")
              content))))

(defun claudecode--session-buffer ()
  "Return the Claude session buffer for the current project, starting one if needed."
  (let* ((proj (project-current nil))
         (name (claudecode--buffer-name proj))
         (buffer (get-buffer name)))
    (unless (buffer-live-p buffer)
      (save-window-excursion
        (claudecode-start))
      (setq buffer (get-buffer name)))
    buffer))

(defun claudecode--send-prompt (prompt)
  "Send PROMPT to the active Claude Code CLI session."
  (let ((text (string-trim-right prompt)))
    (when (string-empty-p text)
      (user-error "[Claude] Cannot send an empty prompt"))
    (let ((buffer (claudecode--session-buffer)))
      (unless (buffer-live-p buffer)
        (user-error "[Claude] Unable to start Claude session"))
      (display-buffer buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        ;; Send string to eat terminal
        (when-let* ((terminal (bound-and-true-p eat-terminal)))
          (eat-term-send-string terminal text)
          (eat-term-send-string terminal "\n")))
      (message "[Claude] Sent prompt (%d chars)" (length text)))))

;;
;; Code change prompt
;;

(defun claudecode--gather-context ()
  "Collect contextual details for the current buffer.
Returns a plist containing file, function, region and defun metadata."
  (let* ((region-active (use-region-p))
         (region-beg (and region-active (region-beginning)))
         (region-end (and region-active (region-end)))
         (defun-bounds (unless region-active (bounds-of-thing-at-point 'defun)))
         (start (or region-beg (car defun-bounds) (line-beginning-position)))
         (end (or region-end (cdr defun-bounds) (line-end-position)))
         (snippet (buffer-substring-no-properties start end))
         (file-name (claudecode--relative-file-name))
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

(defun claudecode--format-location (context)
  "Format a human-readable location string from CONTEXT."
  (let ((file (or (plist-get context :relative-file) "(buffer)"))
        (start (plist-get context :snippet-start))
        (end (plist-get context :snippet-end)))
    (format "%s:%d-%d" file start end)))

(defconst claudecode--code-change-default-postamble
  '("Apply the requested change and explain the modifications you made."
    "Re-run the relevant automated tests and update them if they fail."
    "If the request is ambiguous, ask for clarification before proceeding.")
  "Default trailing instructions appended to Claude code-change prompts.")

(defvar-local claudecode-last-intent nil
  "Symbol describing the most recent Claude intent triggered from this buffer.")

(defun claudecode--code-change (initial &optional postamble intent)
  "Internal helper to prompt for a code change using INITIAL text.
POSTAMBLE, when non-nil, should be a list of trailing instructions.
INTENT is recorded in `claudecode-last-intent' for downstream hooks."
  (unless buffer-file-name
    (user-error "[Claude] Current buffer is not visiting a file"))
  (let* ((context (claudecode--gather-context))
         (desc (string-trim (read-from-minibuffer "Describe the change: " (or initial "")))))
    (when (string-empty-p desc)
      (user-error "[Claude] Change description cannot be empty"))
    (setq claudecode-last-intent intent)
    (let* ((location (claudecode--format-location context))
           (function-name (plist-get context :current-function))
           (function-line (when function-name (format "Function: %s" function-name)))
           (code-block
            (claudecode--format-text-block
             "Current code"
             (claudecode--clip-text (plist-get context :snippet))
             (claudecode--code-fence-language)))
           (post-lines (or postamble claudecode--code-change-default-postamble))
           (post-text (when post-lines (string-join post-lines "\n")))
           (segments (delq nil (list desc
                                     (format "Context: %s" location)
                                     function-line
                                     code-block
                                     post-text)))
           (prompt (string-join segments "\n\n")))
      (claudecode--send-prompt prompt)
      (message "[Claude] Requested code change for %s" location))))

;;;###autoload
(defun claudecode-code-change ()
  "Collect code context and prompt Claude to make a targeted change."
  (interactive)
  (claudecode--code-change nil nil 'custom))

(defun claudecode-code-change-add-logging ()
  "Ask Claude to add structured logging to the current code context."
  (interactive)
  (claudecode--code-change
   "Add structured logging that captures key inputs, outputs, and exceptional conditions without changing the behavior. Use existing logging utilities if available."
   nil
   'logging))

(defun claudecode-code-change-refactor-readability ()
  "Ask Claude to refactor the current code for readability."
  (interactive)
  (claudecode--code-change
   "Refactor this code to improve readability and clarity. Reduce duplication, rename unclear identifiers, and reorganize logic while preserving observable behavior."
   nil
   'refactor-readability))

(defun claudecode-code-change-simplify ()
  "Ask Claude to simplify the selected code."
  (interactive)
  (claudecode--code-change
   "Simplify this code by reducing unnecessary branching or complexity while keeping the same behavior and edge-case handling."
   nil
   'simplify))

(defun claudecode-code-change-add-docs ()
  "Ask Claude to add documentation and comments to the current code."
  (interactive)
  (claudecode--code-change
   "Add concise documentation and inline comments that clarify the purpose, inputs, outputs, and important invariants of this code. Avoid redundant comments."
   nil
   'add-docs))

(defun claudecode-code-change-describe ()
  "Ask Claude to describe what the current code does without modifying it."
  (interactive)
  (claudecode--code-change
   "Describe in detail what this code does, including inputs, outputs, side effects, and tricky edge cases. Highlight any risks or assumptions."
   '("Provide an explanation only; do not modify the code or run commands.")
   'describe))

(defun claudecode-code-change-write-tests ()
  "Ask Claude to write or update tests for the current code."
  (interactive)
  (claudecode--code-change
   "Write or update automated tests that thoroughly cover this behavior, including success, failure, and edge cases."
   '("Focus on creating or updating automated tests for the described behavior."
     "Run the relevant test suite and report the results.")
   'write-tests))

;;
;; Implement TODO prompt
;;

(defun claudecode--is-comment-line (line)
  "Return non-nil when LINE looks like a comment in the current mode."
  (when comment-start
    (let ((prefix (string-trim-right comment-start)))
      (string-match-p (concat "^[\t ]*" (regexp-quote prefix) "+")
                      (string-trim-left (or line ""))))))

;;;###autoload
(defun claudecode-implement-todo (arg)
  "Prepare a Claude prompt to implement TODO style comments.
With prefix ARG, add implementations after comments instead of replacing them."
  (interactive "P")
  (unless buffer-file-name
    (user-error "[Claude] Current buffer is not visiting a file"))
  (setq claudecode-last-intent 'implement-todo)
  (let* ((context (claudecode--gather-context))
         (current-line (string-trim (or (thing-at-point 'line t) "")))
         (current-line-number (line-number-at-pos (point)))
         (is-comment (claudecode--is-comment-line current-line))
         (function-name (plist-get context :current-function))
         (function-context (if function-name (format "\nFunction: %s" function-name) ""))
         (region-active (plist-get context :region-active))
         (region-text (when region-active (claudecode--clip-text (plist-get context :region-text))))
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
    (claudecode--send-prompt prompt)
    (message "[Claude] Requested TODO implementation"))))

;;;###autoload
(defun claudecode-fix-tests ()
  "Send a prompt instructing Claude to run and fix the project's tests.
The active region is treated as failure context and included in the prompt."
  (interactive)
  (setq claudecode-last-intent 'fix-tests)
  (let* ((context (claudecode--gather-context))
         (failure-snippet (when (plist-get context :region-active)
                            (claudecode--clip-text (plist-get context :region-text))))
         (failure-block (claudecode--format-text-block "Failure context" failure-snippet "text"))
         (default (concat "Determine how to run the project's automated test suite from the repository root."
                          "\nRun the tests, capture any failures, diagnose the root causes, apply the minimal code changes needed to fix them, and rerun the tests until they pass."
                          "\nExplain the commands you executed, list failing cases before and after fixes, and summarize the final status."
                          "\nIf the information provided is insufficient, ask clarifying questions before making changes."
                          (claudecode--context-block context nil)
                          (or failure-block "")
                          "\nMaintain existing behavior outside the failing scenarios and avoid unrelated edits."))
         (prompt (read-from-minibuffer "Fix tests prompt: " default)))
    (claudecode--send-prompt prompt)
    (message "[Claude] Requested test fix workflow")))

;;
;; Transient menu
;;

;;;###autoload
(transient-define-prefix claudecode-command-menu ()
  "Transient menu for common Claude Code CLI prompts."
  [["Code change"
    ("c" "Custom" claudecode-code-change)
    ("l" "Add logging" claudecode-code-change-add-logging)
    ("r" "Refactor readability" claudecode-code-change-refactor-readability)
    ("s" "Simplify" claudecode-code-change-simplify)
    ("a" "Add docs/comments" claudecode-code-change-add-docs)
    ("e" "Describe" claudecode-code-change-describe)
    ("w" "Write tests" claudecode-code-change-write-tests)]
   ["Other"
    ("t" "Implement TODO" claudecode-implement-todo)
    ("f" "Fix tests" claudecode-fix-tests)
    ("u" "Unstick terminal" claudecode-unstick-terminal)]])

;;;###autoload
(defun claudecode-start (&optional restart)
  "Start or switch to the per-project Claude Code CLI eat buffer.

With prefix argument RESTART (\\[universal-argument]), restart the Claude
process for the current project (kill if running, then start anew)."
  (interactive "P")
  (let* ((proj (project-current))
         (name (claudecode--buffer-name proj))
         (buf (get-buffer name)))
    (cond
     ;; Restart requested: kill and recreate.
     (restart
      (when (buffer-live-p buf)
        (when-let* ((proc (get-buffer-process buf)))
          (ignore-errors (kill-process proc)))
        (kill-buffer buf))
      (pop-to-buffer-same-window (claudecode--start-process proj)))
     ;; Buffer exists: just switch to it.
     ((buffer-live-p buf)
      (pop-to-buffer-same-window buf))
     ;; Otherwise, create a new one.
     (t
      (pop-to-buffer-same-window (claudecode--start-process proj))))))

(provide 'claudecode)

;;; claudecode.el ends here
