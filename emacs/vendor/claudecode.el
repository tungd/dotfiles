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
(declare-function eat--adjust-process-window-size "eat")

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

(defcustom claudecode-min-window-width 85
  "Minimum width of the Claude Code terminal to prevent autocomplete flickering."
  :type 'integer
  :group 'claudecode)

(defcustom claudecode-notify-on-await t
  "Whether to show a system notification when Claude Code is awaiting the user.
When non-nil, display an OS notification popup when Claude completes a task.
When nil, no notification is shown (silent operation)."
  :type 'boolean
  :group 'claudecode)

(defcustom claudecode-notification-sound "Submarine"
  "The sound to use when displaying system notifications on macOS.
System sounds include: Basso, Blow, Bottle, Frog, Funk, Glass, Hero,
Morse, Ping, Pop, Purr, Sosumi, Submarine, Tink."
  :type 'string
  :group 'claudecode)

;;
;; Claude buffer minor mode
;;

(defvar-local claudecode--resize-pending nil
  "Non-nil when a terminal resize is pending.")

(defvar-local claudecode--last-window-size nil
  "Last known window size as (width . height), to detect actual size changes.")

(defvar-local claudecode--last-scroll-cursor nil
  "Last cursor position used for scroll sync, to avoid redundant recentering.")

(defvar-local claudecode--user-input-pending nil
  "Non-nil when user recently typed input, cleared after scroll sync.")

(defun claudecode--mark-user-input ()
  "Mark that user input just happened, triggering scroll sync on next update."
  (when (bound-and-true-p claudecode-mode)
    (setq claudecode--user-input-pending t)))

(defvar claudecode--resize-idle-timer nil
  "Global idle timer for processing pending resizes.")

(defcustom claudecode-resize-idle-delay 1.0
  "Seconds of idle time before processing pending terminal resizes."
  :type 'number
  :group 'claudecode)


(defun claudecode--do-resize (&optional min-width)
  "Perform terminal resize.
If MIN-WIDTH is provided, ensure terminal is at least that wide.
Otherwise use current window dimensions."
  (when-let* ((terminal (bound-and-true-p eat-terminal)))
    (let* ((win-width (window-body-width))
           (win-height (window-body-height))
           (width (if min-width (max win-width min-width) win-width))
           (height win-height)
           (inhibit-read-only t))
      (eat-term-resize terminal width height)
      (when (fboundp 'eat-term-redisplay)
        (eat-term-redisplay terminal))
      (message "[Claude] Terminal resized to %dx%d (window was %dx%d)"
               width height win-width win-height))))

(defun claudecode--process-pending-resizes ()
  "Process any pending resize requests in Claude buffers."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and (bound-and-true-p claudecode-mode)
                   claudecode--resize-pending)
          (setq claudecode--resize-pending nil)
          (claudecode--do-resize))))))

(defun claudecode--request-resize (_process _windows)
  "Request a deferred resize only if window size actually changed.
Returns nil to skip resize, or the new size to resize immediately."
  (let* ((width (window-body-width))
         (height (window-body-height))
         (new-size (cons width height))
         (size-changed (not (equal new-size claudecode--last-window-size))))
    (if size-changed
        (progn
          ;; Size actually changed - defer the resize
          (setq claudecode--last-window-size new-size)
          (setq claudecode--resize-pending t)
          (unless claudecode--resize-idle-timer
            (setq claudecode--resize-idle-timer
                  (run-with-idle-timer claudecode-resize-idle-delay t
                                       #'claudecode--process-pending-resizes)))
          ;; Return nil to skip immediate resize
          nil)
      ;; Size unchanged - no resize needed, return nil
      nil)))


(define-minor-mode claudecode-mode
  "Minor mode for Claude Code CLI buffers.

Configures buffer-local settings optimized for the Claude Code terminal,
including display settings, scroll behavior, and performance tweaks."
  :init-value nil
  :lighter " Claude"
  :group 'claudecode
  (when claudecode-mode
    ;; Scroll settings - prevent recentering and smooth scrolling
    (setq-local scroll-margin 0)
    (setq-local scroll-conservatively 10000)
    (setq-local maximum-scroll-margin 0)
    (setq-local scroll-preserve-screen-position t)
    (setq-local auto-window-vscroll nil)
    (setq-local fringe-mode 0)

    ;; Explicitly use default resize function (eat or something else might override)
    (setq-local window-adjust-process-window-size-function
                #'window-adjust-process-window-size-smallest)

    ;; Replace blinking indicator character to prevent height fluctuations
    (let ((display-table (make-display-table)))
      (aset display-table #x23fa [?✽])  ; Replace ⏺ (U+23FA) with ✽
      (setq-local buffer-display-table display-table))

    ;; Disable blinking text (causes constant redraws)
    (when (boundp 'eat-enable-blinking-text)
      (setq-local eat-enable-blinking-text nil))

    ;; Mode-line optimization (minimal mode-line to reduce redisplay cost)
    (setq-local mode-line-format
                '(" " mode-line-buffer-identification
                  " " mode-line-misc-info))

    ;; Disable cursor blinking
    (setq-local cursor-type 'box)
    (when (bound-and-true-p blink-cursor-mode)
      (setq-local blink-cursor-mode nil))

    ;; Font cache optimization
    (setq-local inhibit-compacting-font-caches t)

    ;; Reduce redisplay frequency
    (setq-local redisplay-skip-fontification-on-input t)
    (setq-local fast-but-imprecise-scrolling t)

    ;; Disable expensive global minor modes
    (when (bound-and-true-p hl-line-mode)
      (hl-line-mode -1))
    (when (bound-and-true-p display-line-numbers-mode)
      (display-line-numbers-mode -1))
    (when (bound-and-true-p diff-hl-mode)
      (diff-hl-mode -1))
    (when (bound-and-true-p show-paren-mode)
      (show-paren-local-mode -1))
    (when (bound-and-true-p pixel-scroll-precision-mode)
      (setq-local pixel-scroll-precision-mode nil))

    ;; Disable eldoc (not useful in terminal)
    (eldoc-mode -1)

    ;; Eat terminal optimizations
    (setq-local eat-maximum-latency 0.064)
    (setq-local eat-minimum-latency 0.001)
    (setq-local eat-term-scrollback-size (* 2 1024 1024))
    (setq-local eat-enable-shell-prompt-annotation nil)
    (setq-local eat-input-chunk-size 4096)
    ;; Custom scroll sync to prevent jumping
    (setq-local eat--synchronize-scroll-function #'claudecode--synchronize-scroll)
    ;; Track user input to only scroll sync when user types
    (add-hook 'post-command-hook #'claudecode--mark-user-input nil t)
    ))

(defun claudecode-unstick-terminal ()
  "Force redisplay to fix visual artifacts or stuck display.
Use this when the terminal display becomes corrupted."
  (interactive)
  (setq claudecode--resize-pending nil)
  ;; Reset caches so next operations work properly
  (setq claudecode--last-scroll-cursor nil)
  (setq claudecode--last-window-size nil)
  (claudecode--do-resize)
  ;; Force a proper scroll sync after resize
  (when (bound-and-true-p eat-terminal)
    (let ((cursor-pos (eat-term-display-cursor eat-terminal)))
      (goto-char cursor-pos)
      (recenter -1)))
  (redisplay t)
  (message "[Claude] Terminal display reset"))

;;
;; Notification system (bell handler)
;;

(defun claudecode--system-notification (message &optional title)
  "Show a macOS system notification with MESSAGE and optional TITLE."
  (let ((title (or title "Claude Code"))
        (message (or message "Claude Code is awaiting your input")))
    (call-process "osascript" nil nil nil
                  "-e" (format "display notification \"%s\" with title \"%s\" sound name \"%s\""
                               message title claudecode-notification-sound))))

(defun claudecode--bell-handler (_terminal)
  "Handle bell events from Claude Code.
Called when Claude Code sends a bell character, indicating it's waiting for input."
  (when claudecode-notify-on-await
    (claudecode--system-notification "Claude finished and is awaiting your input")))

;;;###autoload
(defun claudecode-setup-bell-handler ()
  "Set up the completion notification handler for the current Claude buffer.
Use this interactively if notifications aren't working after starting a session."
  (interactive)
  (when-let* ((terminal (bound-and-true-p eat-terminal)))
    (setf (eat-term-parameter terminal 'ring-bell-function)
          #'claudecode--bell-handler)
    (message "[Claude] Bell handler configured for notifications")))

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
      (claudecode-mode 1)
      ;; Force initial resize with minimum width to prevent autocomplete flickering
      ;; and set up bell handler for notifications
      (let ((buf (current-buffer)))
        (run-with-timer
         0.5 nil
         (lambda ()
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (claudecode--do-resize claudecode-min-window-width)
               (claudecode-setup-bell-handler)))))))
    buffer-name))

(defun claudecode--synchronize-scroll (windows)
  "Custom scroll sync to keep prompt at bottom of WINDOWS.
Replaces eat's default scroll function to prevent jumping to buffer
beginning when switching buffers.
Only recenters when user has typed (not when Claude updates output)."
  (dolist (window windows)
    (cond
     ;; Handle special 'buffer symbol (from eat's scroll-windows function)
     ((eq window 'buffer)
      (when (bound-and-true-p eat-terminal)
        (goto-char (eat-term-display-cursor eat-terminal))))
     ;; Handle actual windows
     ((and (windowp window)
           (bound-and-true-p eat-terminal))
      (let* ((cursor-pos (eat-term-display-cursor eat-terminal))
             (cursor-line (line-number-at-pos cursor-pos))
             (last-line (and claudecode--last-scroll-cursor
                             (line-number-at-pos claudecode--last-scroll-cursor)))
             ;; Only recenter if cursor moved to a different line
             (cursor-line-changed (or (null last-line)
                                      (/= cursor-line last-line))))
        (set-window-point window cursor-pos)
        ;; Only recenter when user typed AND cursor line changed
        ;; This prevents flickering when Claude updates output
        (when (and claudecode--user-input-pending cursor-line-changed)
          (setq claudecode--last-scroll-cursor cursor-pos)
          (setq claudecode--user-input-pending nil)
          (with-selected-window window
            (when (>= cursor-pos (- (point-max) 100))
              (recenter -1)))))))))

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
    ("u" "Unstick terminal" claudecode-unstick-terminal)
    ("n" "Setup notifications" claudecode-setup-bell-handler)]])

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
