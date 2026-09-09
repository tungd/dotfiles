;;; scv-drepl.el --- SCV kernel for dREPL -*- lexical-binding: t; -*-

;;; Commentary:
;; Run the ordinary SCV command as a hidden dREPL frontend in the current
;; project.  dREPL itself remains an external GNU ELPA dependency.

;;; Code:

(require 'cl-lib)
(require 'ansi-color)
(require 'json)
(require 'subr-x)
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

(defvar-local drepl-scv--todo nil
  "Latest SCV todo summary.")

(defvar-local drepl-scv--ansi-context nil
  "ANSI decoder state carried between process-output chunks.")

(defvar-local drepl-scv--busy-started-at nil)
(defvar-local drepl-scv--status-timer nil)
(defvar-local drepl-scv--steering-prompt-range nil)
(defvar-local drepl-scv--steering-prompt-timer nil)

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
  (setq-local header-line-format nil)
  (unless (member '(:eval (drepl-scv--render-mode-line-status))
                  mode-line-format)
    (setq-local mode-line-format
                (append mode-line-format
                        '((:eval (drepl-scv--render-mode-line-status))))))
  (add-hook 'post-self-insert-hook #'drepl-scv--maybe-complete-slash nil t)
  (add-hook 'pre-command-hook
            #'drepl-scv--ensure-steering-prompt-before-command nil t)
  (add-hook 'comint-preoutput-filter-functions
            #'drepl-scv--move-steering-prompt-before-output nil t)
  (add-hook 'comint-preoutput-filter-functions
            #'drepl-scv--decode-ansi-output t t)
  (add-hook 'comint-output-filter-functions
            #'drepl-scv--restore-steering-prompt-after-output t t)
  (add-hook 'kill-buffer-hook #'drepl-scv--stop-live-timers nil t))

(defun drepl-scv--decode-ansi-output (output)
  "Decode ANSI in process OUTPUT, carrying partial sequences across chunks."
  (let ((ansi-color-context drepl-scv--ansi-context))
    (prog1 (ansi-color-apply output)
      (setq drepl-scv--ansi-context ansi-color-context))))

(defun drepl-scv--move-steering-prompt-before-output (output)
  "Remove the movable busy prompt before inserting process OUTPUT.

If the stored markers do not belong to the current buffer (e.g. because a
timer fired in a different REPL buffer), they are garbage-collected without
touching the buffer."
  (when drepl-scv--steering-prompt-range
    (pcase-let ((`(,start . ,end) drepl-scv--steering-prompt-range))
      (if (and (eq (marker-buffer start) (current-buffer))
               (eq (marker-buffer end) (current-buffer)))
          (let ((inhibit-read-only t))
            (delete-region start end))
        ;; Markers belong to a dead or foreign buffer — discard silently.
        (set-marker start nil)
        (set-marker end nil))
      (setq drepl-scv--steering-prompt-range nil)))
  output)

(defun drepl-scv--schedule-steering-prompt (repl)
  "Schedule REPL's movable prompt after the current process filter returns."
  (when (timerp drepl-scv--steering-prompt-timer)
    (cancel-timer drepl-scv--steering-prompt-timer))
  (setq drepl-scv--steering-prompt-timer
        (run-at-time 0 nil #'drepl-scv--insert-steering-prompt repl)))

(defun drepl-scv--restore-steering-prompt-after-output (output)
  "Restore the movable busy prompt after process OUTPUT."
  (when (and drepl--current
             (eq (drepl--status drepl--current) 'busy)
             (not (equal output "› ")))
    (drepl-scv--schedule-steering-prompt drepl--current)))

(defun drepl-scv--ensure-steering-prompt-before-command ()
  "Make the busy composer editable before a command at process output end."
  (when-let* ((repl drepl--current)
              (process (drepl--process repl))
              ((eq (drepl--status repl) 'busy))
              ((>= (point) (process-mark process))))
    (drepl-scv--insert-steering-prompt repl)))

(defun drepl-scv--insert-steering-prompt (repl)
  "Keep REPL editable while REPL is processing an active turn.

This function may be called from a timer in any buffer, so it explicitly
switches to the REPL's buffer to update the correct buffer-local state.
If the REPL's buffer has been killed the call is a no-op."
  (let ((buffer (drepl--buffer repl)))
    (unless (buffer-live-p buffer)
      (cl-return-from drepl-scv--insert-steering-prompt))
    (with-current-buffer buffer
      (setq drepl-scv--steering-prompt-timer nil)
      (when-let* ((process (drepl--process repl))
                  ((process-live-p process))
                  ((eq (drepl--status repl) 'busy))
                  ((not ansi-osc--marker)))
        (unless drepl-scv--steering-prompt-range
          (let ((start (copy-marker (process-mark process))))
            (comint-output-filter process "› ")
            (setq drepl-scv--steering-prompt-range
                  (cons start (copy-marker (process-mark process))))))))))

(cl-defmethod drepl--eval ((repl drepl-scv) code)
  "Send CODE immediately so SCV can steer an active turn."
  (if (eq (drepl--status repl) 'busy)
      (let* ((id (cl-incf (drepl--last-id repl)))
             (data `(:id ,id :op "eval" :code ,code)))
        (push (cons id #'ignore) (drepl--callbacks repl))
        (drepl--send-request repl data))
    (cl-call-next-method))
  (drepl-scv--schedule-steering-prompt repl))

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
                          ((equal command "/skill") "Skill: ")
                          ((equal command "/resume-codex") "Codex session: ")
                          ((equal command "/resume-claude") "Claude session: ")
                          ((member command '("/mode"))
                           "Permissions: ")))
            (argument
             (condition-case nil
                 (completing-read prompt candidates nil t)
               (quit nil))))
      (concat command " " argument (if (equal command "/skill") " " ""))
    (if (member command '("/compact" "/mode" "/model"
                          "/resume-codex" "/resume-claude"
                          "/help" "/exit" "/skill"))
        command
      (concat command " "))))

(defun drepl-scv--argument-candidates (command)
  "Return completion candidates for COMMAND's argument."
  (when (member command '("/model" "/mode" "/skill"
                          "/resume-codex" "/resume-claude"))
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

(defun drepl-scv--mode-line-usage ()
  (let* ((ctx (or (alist-get 'context_window drepl-scv--usage) 0))
         (total (or (alist-get 'total_tokens drepl-scv--usage) 0))
         (context (drepl-scv--trim-scale (drepl-scv--scaled-count ctx))))
    (when (> ctx 0)
      (format "%.1f%%/%s" (* 100.0 (/ total (float ctx))) context))))

(defun drepl-scv--mode-line-config ()
  (let ((mode (alist-get 'mode drepl-scv--ui))
        (model (alist-get 'model drepl-scv--ui)))
    (cond
     ((and mode model) (format "%s · %s" mode model))
     (mode mode)
     (model model))))

(defun drepl-scv--mode-line-todo ()
  (let ((total (or (alist-get 'total drepl-scv--todo) 0)))
    (when (> total 0)
      (propertize
       (format "todo:%d/%d"
               (or (alist-get 'completed drepl-scv--todo) 0) total)
       'help-echo (or (alist-get 'current drepl-scv--todo) "")))))

(defun drepl-scv--mode-line-activity (status)
  (let* ((elapsed (if drepl-scv--busy-started-at
                      (max 0 (floor (- (float-time)
                                       drepl-scv--busy-started-at)))
                    0))
         (phase (mod (floor (* 10 (- (float-time)
                                     (or drepl-scv--busy-started-at
                                         (float-time))))) 10)))
    (pcase status
      ('busy (propertize
              (format "%s Working %ds"
                      (aref drepl-scv--spinner-frames phase) elapsed)
              'face 'mode-line-emphasis))
      ('rawio (propertize "Waiting" 'face 'warning))
      (_ nil))))

(defun drepl-scv--render-mode-line-status ()
  "Render compact live SCV activity, todo, and context state."
  (let* ((status (and drepl--current (drepl--status drepl--current)))
         (parts (delq nil
                      (list (drepl-scv--mode-line-activity status)
                            (drepl-scv--mode-line-config)
                            (drepl-scv--mode-line-todo)
                            (drepl-scv--mode-line-usage))))
         (rendered (string-join parts " · ")))
    (if (string-empty-p rendered) ""
      (concat "  " (replace-regexp-in-string "%" "%%" rendered t t)))))

(defun drepl-scv--stop-live-timers ()
  (when (timerp drepl-scv--status-timer)
    (cancel-timer drepl-scv--status-timer))
  (when (timerp drepl-scv--steering-prompt-timer)
    (cancel-timer drepl-scv--steering-prompt-timer))
  (setq drepl-scv--status-timer nil
        drepl-scv--steering-prompt-timer nil
        drepl-scv--steering-prompt-range nil))

(defun drepl-scv--sync-busy-clock (status)
  (if (eq status 'busy)
      (unless drepl-scv--busy-started-at
        (setq drepl-scv--busy-started-at (float-time)
              drepl-scv--status-timer
              (run-at-time 0.1 0.1 #'force-window-update (current-buffer))))
    (setq drepl-scv--busy-started-at nil)
    (drepl-scv--stop-live-timers)))

(defun drepl-scv--update-live-state (repl)
  "Refresh mode-line state for REPL without consuming a header line."
  (let ((buffer (drepl--buffer repl)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq-local header-line-format nil)
        (force-window-update buffer)))))

(cl-defmethod drepl--init :after ((repl drepl-scv))
  (drepl-scv--update-live-state repl))

(cl-defmethod drepl--handle-notification :after ((_repl drepl-scv) _data)
  (when-let* ((buffer (drepl--buffer _repl)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (drepl-scv--sync-busy-clock (drepl--status _repl)))))
  (drepl-scv--update-live-state _repl))

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
    ("scv/todo"
     (let ((buffer (drepl--buffer repl)))
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq-local drepl-scv--todo
                       (mapcar (lambda (key) (cons key (alist-get key data)))
                               '(total completed in_progress pending
                                 current)))))))
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
