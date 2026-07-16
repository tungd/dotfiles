;;; scv-drepl.el --- SCV kernel for dREPL -*- lexical-binding: t; -*-

;;; Commentary:
;; Run the ordinary SCV command as a hidden dREPL frontend in the current
;; project.  dREPL itself remains an external GNU ELPA dependency.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'drepl)

;; dREPL 0.4 still dynamically references this Comint display option, removed
;; in Emacs 31.  Nil preserves `pop-to-buffer' default display behavior.
(defvar display-comint-buffer-action nil)

(defgroup drepl-scv nil
  "SCV kernel for dREPL."
  :group 'drepl)

(defcustom drepl-scv-program "scv"
  "SCV executable used by `drepl-scv'."
  :type 'string
  :group 'drepl-scv)

(defcustom drepl-scv-arguments nil
  "Ordinary SCV options used to start the dREPL kernel.

For example, use (\"-r\" \"SESSION-ID\") to resume.  The frontend is selected
through a process-local environment variable, not a command-line subcommand."
  :type '(repeat string)
  :group 'drepl-scv)

(defvar drepl-scv--pending-questions (make-hash-table :test #'equal)
  "Questions already scheduled outside a dREPL process filter.")

(defvar-local drepl-scv--usage nil
  "Latest provider usage snapshot as an alist, or nil.
Keys: context_window, input_tokens, output_tokens, cached_tokens, total_tokens.")

(defvar-local drepl-scv--ui nil
  "Latest TUI footer state: mode, model, cwd, and git_head.")

(defvar-local drepl-scv--busy-started-at nil)
(defvar-local drepl-scv--header-timer nil)

(defconst drepl-scv--other-choice "Other…")

(defun drepl-scv--with-frontend (function &rest arguments)
  "Call FUNCTION with ARGUMENTS under a process-local SCV frontend setting."
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "SCV_FRONTEND" "drepl")
    (apply function arguments)))

(defun drepl-scv--kernel-supported-p ()
  "Return non-nil when `drepl-scv-program' advertises the hidden frontend."
  (when-let* ((program (executable-find drepl-scv-program)))
    (let ((output (generate-new-buffer " *scv-drepl-capability*")))
      (unwind-protect
          (drepl-scv--with-frontend
           (lambda ()
             (and (eq 0 (call-process program nil output nil "--help"))
                  (with-current-buffer output
                    (goto-char (point-min))
                    (search-forward "SCV dREPL kernel" nil t))
                  t)))
        (kill-buffer output)))))

;;;###autoload (autoload 'drepl-scv "scv-drepl" nil t)
(drepl--define drepl-scv
  :display-name "SCV"
  :docstring "Start SCV through dREPL in the current project.")

(cl-defmethod drepl--command ((_ drepl-scv))
  (unless (drepl-scv--kernel-supported-p)
    (user-error "Installed SCV lacks SCV_FRONTEND=drepl support"))
  (cons drepl-scv-program drepl-scv-arguments))

(cl-defmethod drepl--init ((repl drepl-scv))
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "SCV_FRONTEND" "drepl")
    (cl-call-next-method repl))
  (drepl--adapt-comint-to-mode 'text-mode)
  (setq-local comint-prompt-regexp "^› ")
  (setq-local comint-use-prompt-regexp t)
  (setq-local comint-prompt-read-only t)
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (add-hook 'post-self-insert-hook #'drepl-scv--maybe-complete-slash nil t)
  (add-hook 'kill-buffer-hook #'drepl-scv--stop-header-timer nil t))

(defun drepl-scv--question-key (repl data)
  "Return the unique pending-question key for REPL and DATA."
  (cons (drepl--process repl) (alist-get 'question_id data)))

(defun drepl-scv--schedule-question (repl data)
  "Schedule DATA's SCV question for REPL outside the process filter."
  (let ((key (drepl-scv--question-key repl data)))
    (unless (gethash key drepl-scv--pending-questions)
      (puthash key t drepl-scv--pending-questions)
      (run-at-time 0 nil #'drepl-scv--ask-question repl data key))))

(defun drepl-scv--question-prompt (data)
  "Build a minibuffer prompt from question DATA."
  (let ((header (or (alist-get 'header data) "SCV"))
        (prompt (or (alist-get 'prompt data) "Choose an answer")))
    (format "%s — %s: " header prompt)))

(defun drepl-scv--send-answer (repl id answer)
  "Send REPL one unframed JSON answer line for ID and ANSWER."
  (when-let* ((process (drepl--process repl))
              ((process-live-p process)))
    (process-send-string
     process
     (concat (json-serialize (list :id id :answer answer)) "\n"))))

(defun drepl-scv--read-answer (data)
  "Read an answer for SCV question DATA."
  (let* ((options (alist-get 'options data))
         (labels (mapcar (lambda (option) (alist-get 'label option)) options))
         (allow-custom (alist-get 'allow_custom data))
         (choices (if allow-custom
                      (append labels (list drepl-scv--other-choice))
                    labels))
         (descriptions
          (mapcar (lambda (option)
                    (cons (alist-get 'label option)
                          (alist-get 'description option)))
                  options))
         (completion-extra-properties
          `(:annotation-function
            ,(lambda (choice)
               (when-let* ((description (alist-get choice descriptions nil nil
                                                    #'equal)))
                 (concat "  " description))))))
    (cond
     ((null choices) (or (alist-get 'cancel_answer data) ""))
     ((and allow-custom (null labels))
      (read-string (drepl-scv--question-prompt data)))
     (t
      (let ((choice
             (completing-read (drepl-scv--question-prompt data) choices nil t)))
        (if (equal choice drepl-scv--other-choice)
            (read-string "Answer: ")
          choice))))))

(defun drepl-scv--ask-question (repl data key)
  "Prompt for DATA in REPL, then remove pending question KEY."
  (unwind-protect
      (let* ((cancel-answer (or (alist-get 'cancel_answer data) ""))
             (answer
              (condition-case nil
                  (drepl-scv--read-answer data)
                (quit cancel-answer)
                (error cancel-answer))))
        (drepl-scv--send-answer repl (alist-get 'question_id data) answer))
    (remhash key drepl-scv--pending-questions)))

(defun drepl-scv--maybe-complete-slash ()
  "Read an SCV command when `/` starts the current prompt input."
  (when (eq ?/ last-command-event)
    (let ((input (buffer-substring-no-properties
                  (comint-line-beginning-position) (point))))
      (when (string-match-p "\\`[[:space:]]*/\\'" input)
        (when-let* ((candidates (drepl-scv--slash-command-candidates))
                    (choice
                     (condition-case nil
                         (completing-read "SCV command: " candidates nil t "/")
                       (quit nil))))
          (delete-char -1)
          (insert (drepl-scv--complete-command choice)))))))

(defun drepl-scv--complete-command (command)
  "Complete COMMAND's required argument, when it has a selector."
  (if-let* ((candidates (drepl-scv--argument-candidates command))
            (prompt (cond ((equal command "/model") "Model: ")
                          ((member command '("/mode" "/permissions"))
                           "Permissions: ")))
            (argument
             (condition-case nil
                 (completing-read prompt candidates nil t)
               (quit nil))))
      (concat command " " argument)
    command))

(defun drepl-scv--argument-candidates (command)
  "Return completion candidates for COMMAND's argument."
  (when (member command '("/model" "/mode" "/permissions"))
    (when-let* ((repl (drepl--get-repl 'ready))
                (code (concat command " "))
                (reply (drepl--completion-cadidates repl code (length code))))
      (cdr reply))))

(defun drepl-scv--slash-command-candidates ()
  "Return annotated slash-command candidates from the active SCV kernel."
  (when-let* ((repl (drepl--get-repl 'ready))
              (reply (drepl--completion-cadidates repl "/" 1)))
    (cdr reply)))

(defconst drepl-scv--spinner-frames ["⠋" "⠙" "⠹" "⠸" "⠼"
                                      "⠴" "⠦" "⠧" "⠇" "⠏"])

(defun drepl-scv--scaled-count (value)
  "Format integer VALUE using the TUI token-count convention."
  (cond
   ((>= value 1000000) (format "%.1fM" (/ value 1000000.0)))
   ((>= value 1000) (format "%.1fk" (/ value 1000.0)))
   (t (number-to-string value))))

(defun drepl-scv--trim-scale (value)
  (replace-regexp-in-string "\\.0\\([kM]\\)\\'" "\\1" value))

(defun drepl-scv--footer-left ()
  (let* ((mode (or (alist-get 'mode drepl-scv--ui) "ask"))
         (model (or (alist-get 'model drepl-scv--ui) ""))
         (cwd (or (alist-get 'cwd drepl-scv--ui) default-directory))
         (cwd (directory-file-name (abbreviate-file-name cwd)))
         (git (or (alist-get 'git_head drepl-scv--ui) "")))
    (concat mode " · " model " · " cwd
            (if (string-empty-p git) "" (format " [%s]" git)))))

(defun drepl-scv--footer-right ()
  (let* ((ctx (or (alist-get 'context_window drepl-scv--usage) 0))
         (total (or (alist-get 'total_tokens drepl-scv--usage) 0))
         (input (or (alist-get 'input_tokens drepl-scv--usage) 0))
         (output (or (alist-get 'output_tokens drepl-scv--usage) 0))
         (cached (or (alist-get 'cached_tokens drepl-scv--usage) 0))
         (context (if (> ctx 0)
                      (format "%.1f%%/%s" (* 100.0 (/ total (float ctx)))
                              (drepl-scv--trim-scale
                               (drepl-scv--scaled-count ctx)))
                    "-/-"))
         (cache-pct (if (> input 0) (* 100.0 (/ cached (float input))) 0.0)))
    (format "%s · ↑%s/↓%s (%.1f%%)" context
            (drepl-scv--trim-scale (drepl-scv--scaled-count input))
            (drepl-scv--trim-scale (drepl-scv--scaled-count output))
            cache-pct)))

(defun drepl-scv--format-usage (usage)
  "Return a compact usage suffix for header from USAGE alist, or empty string."
  (if-let* ((total (alist-get 'total_tokens usage))
            (ctx (alist-get 'context_window usage))
            ((numberp total))
            ((numberp ctx)))
      (let* ((input (or (alist-get 'input_tokens usage) 0))
             (output (or (alist-get 'output_tokens usage) 0))
             (cached (or (alist-get 'cached_tokens usage) 0))
             (pct (if (> ctx 0) (/ (* total 100) ctx) 0)))
        (format "  %d/%d (%d%%)" total ctx pct))
    ""))

(defun drepl-scv--format-header (status)
  "Return TUI-style (LEFT RIGHT) header text for STATUS."
  (let* ((elapsed (if drepl-scv--busy-started-at
                      (max 0 (floor (- (float-time)
                                       drepl-scv--busy-started-at)))
                    0))
         (phase (mod (floor (* 10 (- (float-time)
                                     (or drepl-scv--busy-started-at
                                         (float-time))))) 10))
         (state
          (pcase status
            ('busy (format "%s Working… (%ds) · "
                           (aref drepl-scv--spinner-frames phase) elapsed))
            ('rawio "Waiting… · ")
            (_ ""))))
    (list (concat state (drepl-scv--footer-left))
          (drepl-scv--footer-right))))

(defun drepl-scv--render-header-line ()
  "Render the live SCV header with its right side aligned like the TUI."
  (pcase-let ((`(,left ,right)
               (drepl-scv--format-header
                (and drepl--current (drepl--status drepl--current)))))
    (list " " (replace-regexp-in-string "%" "%%" left t t)
          (propertize " " 'display
                      `(space :align-to (- right-fringe ,(1+ (string-width right)))))
          (replace-regexp-in-string "%" "%%" right t t) " ")))

(defun drepl-scv--stop-header-timer ()
  (when (timerp drepl-scv--header-timer)
    (cancel-timer drepl-scv--header-timer))
  (setq drepl-scv--header-timer nil))

(defun drepl-scv--sync-busy-clock (status)
  (if (eq status 'busy)
      (unless drepl-scv--busy-started-at
        (setq drepl-scv--busy-started-at (float-time)
              drepl-scv--header-timer
              (run-at-time 0.1 0.1 #'force-window-update (current-buffer))))
    (setq drepl-scv--busy-started-at nil)
    (drepl-scv--stop-header-timer)))

(defun drepl-scv--update-header (repl)
  "Update `header-line-format' from REPL status and force redisplay."
  (let ((buffer (drepl--buffer repl)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq-local header-line-format '(:eval (drepl-scv--render-header-line)))
        (force-window-update buffer)))))

(cl-defmethod drepl--init :after ((repl drepl-scv))
  (drepl-scv--update-header repl))

(cl-defmethod drepl--handle-notification :after ((_repl drepl-scv) _data)
  (when-let* ((buffer (drepl--buffer _repl)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (drepl-scv--sync-busy-clock (drepl--status _repl)))))
  (drepl-scv--update-header _repl))

(cl-defmethod drepl--handle-notification ((repl drepl-scv) data)
  (pcase (alist-get 'op data)
    ("scv/question"
     (drepl-scv--schedule-question repl data))
    ("scv/usage"
     (let ((buffer (drepl--buffer repl)))
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq-local drepl-scv--usage
                       (mapcar (lambda (key)
                                 (cons key (alist-get key data)))
                               '(context_window input_tokens
                                 output_tokens cached_tokens total_tokens)))))))
    ("scv/ui"
     (let ((buffer (drepl--buffer repl)))
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq-local drepl-scv--ui
                       (mapcar (lambda (key) (cons key (alist-get key data)))
                               '(mode model cwd git_head)))))))
    (_
     (cl-call-next-method))))

(provide 'scv-drepl)
;;; scv-drepl.el ends here
