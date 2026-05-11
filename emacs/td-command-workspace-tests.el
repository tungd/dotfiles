;;; td-command-workspace-tests.el --- Tests for command workspace -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)

(setq load-prefer-newer t)

(add-to-list 'load-path
             (file-name-directory (or load-file-name buffer-file-name)))
(require 'td-command-workspace)

(defun td/command-workspace-test--permission-request-event ()
  "Return a representative permission_request CLI Agent Event."
  '(("event" . "permission_request")
    ("session_id" . "sess-1")
    ("request_id" . "req-1")
    ("tool_name" . "bash")
    ("project" . "dotfiles")
    ("cwd" . "/tmp/dotfiles/")
    ("input_preview" . "make test")))

(defun td/command-workspace-test--permission-replied-event (&optional decision)
  "Return a representative permission_replied CLI Agent Event."
  `(("event" . "permission_replied")
    ("session_id" . "sess-1")
    ("request_id" . "req-1")
    ("decision" . ,(or decision "approve"))
    ("project" . "dotfiles")
    ("cwd" . "/tmp/dotfiles/")))

(defun td/command-workspace-test--question-asked-event ()
  "Return a representative question_asked CLI Agent Event."
  '(("event" . "question_asked")
    ("session_id" . "sess-1")
    ("question_id" . "q-1")
    ("question" . "Which migration path should I use?")
    ("context" . "Two data migrations are possible.")
    ("project" . "dotfiles")
    ("cwd" . "/tmp/dotfiles/")))

(defun td/command-workspace-test--stop-event ()
  "Return a representative stop CLI Agent Event."
  '(("event" . "stop")
    ("session_id" . "sess-1")
    ("event_id" . "evt-stop-1")
    ("summary" . "Implemented the requested change.")
    ("project" . "dotfiles")
    ("cwd" . "/tmp/dotfiles/")))

(defun td/command-workspace-test--session-error-event ()
  "Return a representative session_error CLI Agent Event."
  '(("event" . "session_error")
    ("session_id" . "sess-1")
    ("event_id" . "evt-error-1")
    ("message" . "td-agent exited with status 2")
    ("project" . "dotfiles")
    ("cwd" . "/tmp/dotfiles/")))

(defun td/command-workspace-test--session-start-event ()
  "Return a representative session_start CLI Agent Event."
  '(("event" . "session_start")
    ("session_id" . "sess-1")
    ("task_title" . "Inspect workspace")
    ("task_title_source" . "explicit")
    ("project" . "dotfiles")
    ("cwd" . "/tmp/dotfiles/")))

(defun td/command-workspace-test--reset-agent-state ()
  "Clear shared Agent Notebook, action, queue, and notification state."
  (clrhash td/command-workspace--agent-permission-requests)
  (clrhash td/command-workspace--agent-questions)
  (clrhash td/command-workspace--agent-notebook-session-buffers)
  (clrhash td/command-workspace--agent-notifications)
  (setq td/command-workspace--agent-notification-order nil)
  (setq td/command-workspace--agent-action-event nil)
  (dolist (buffer-name '("*td-agent action queue*" "*td-agent notification*"))
    (when-let* ((buffer (get-buffer buffer-name)))
      (kill-buffer buffer))))

(defmacro td/command-workspace-test--with-notebook (&rest body)
  "Run BODY in a temporary Agent Notebook buffer."
  `(let ((td/command-workspace-agent-auto-open-action-transient nil))
     (with-temp-buffer
       (td/command-workspace-agent-notebook-mode)
       (setq-local td/command-workspace--agent-notebook-project-root
                   "/tmp/dotfiles/")
       ,@body)))

(defun td/command-workspace-test--count-matches (regexp text)
  "Return the number of REGEXP matches in TEXT."
  (let ((start 0)
        (count 0))
    (while (string-match regexp text start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    count))

(defun td/command-workspace-test--write-jsonl (file events)
  "Write EVENTS as JSONL to FILE."
  (with-temp-file file
    (dolist (event events)
      (insert (json-encode event) "\n"))))

(defun td/command-workspace-test--canonical-session-events ()
  "Return representative canonical transcript events for one complete cell."
  (list
   '(("event" . "session_start")
     ("session_id" . "sess-1")
     ("task_title" . "Inspect workspace")
     ("task_title_source" . "explicit")
     ("project" . "dotfiles")
     ("cwd" . "/tmp/dotfiles/"))
   '(("event" . "prompt_submit")
     ("session_id" . "sess-1")
     ("query" . "Inspect repository state")
     ("project" . "dotfiles")
     ("cwd" . "/tmp/dotfiles/"))
   '(("event" . "tool_start")
     ("session_id" . "sess-1")
     ("tool_name" . "bash")
     ("tool_input" . "make test")
     ("project" . "dotfiles")
     ("cwd" . "/tmp/dotfiles/"))
   '(("event" . "tool_complete")
     ("session_id" . "sess-1")
     ("tool_name" . "bash")
     ("tool_input" . "make test")
     ("response" . "canonical tool output")
     ("project" . "dotfiles")
     ("cwd" . "/tmp/dotfiles/"))
   '(("event" . "stop")
     ("session_id" . "sess-1")
     ("summary" . "Canonical summary from event log.")
     ("project" . "dotfiles")
     ("cwd" . "/tmp/dotfiles/"))
   '(("event" . "idle_prompt")
     ("session_id" . "sess-1")
     ("status" . "ready_for_prompt")
     ("project" . "dotfiles")
     ("cwd" . "/tmp/dotfiles/"))))

(ert-deftest td-command-workspace-new-agent-notebook-submits-titled-run ()
  (td/command-workspace-test--reset-agent-state)
  (let (made-process process-put-call opened-root popped-buffer)
    (cl-letf (((symbol-function 'td/command-workspace--agent-executable)
               (lambda () "/tmp/bin/td-agent"))
              ((symbol-function 'make-process)
               (lambda (&rest plist)
                 (setq made-process plist)
                 'fake-process))
              ((symbol-function 'process-put)
               (lambda (process property value)
                 (setq process-put-call (list process property value))
                 value))
              ((symbol-function 'td/command-workspace-open-project-workspace)
               (lambda (&optional project-root _file)
                 (setq opened-root project-root)))
              ((symbol-function 'pop-to-buffer)
               (lambda (buffer-or-name &rest _args)
                 (setq popped-buffer
                       (if (bufferp buffer-or-name)
                           buffer-or-name
                         (get-buffer buffer-or-name)))
                 popped-buffer)))
      (let ((buffer
             (td/command-workspace-new-agent-notebook
              "Inspect workspace"
              "/tmp/dotfiles/"
              "Inspect repository state")))
        (should (equal opened-root "/tmp/dotfiles/"))
        (should (eq popped-buffer buffer))
        (with-current-buffer buffer
          (should (derived-mode-p 'td/command-workspace-agent-notebook-mode))
          (should (string-match-p "# %% Draft Notebook Cell" (buffer-string)))
          (should (string-match-p ":state: draft" (buffer-string)))
          (should (= 1 (td/command-workspace-test--count-matches
                        ":state: draft" (buffer-string))))
          (goto-char (point-min))
          (forward-char 1)
          (should-error (insert "not editable") :type 'text-read-only)
          (goto-char (point-min))
          (search-forward "Inspect repository state")
          (insert " carefully")
          (td/command-workspace-agent-notebook-submit)
          (should
           (equal (plist-get made-process :command)
                  '("/tmp/bin/td-agent"
                    "run"
                    "--title"
                    "Inspect workspace"
                    "Inspect repository state carefully")))
          (should (eq td/command-workspace--agent-notebook-process
                      'fake-process))
          (should (equal process-put-call
                         '(fake-process td-agent-events nil)))
          (should (string-match-p ":state: running" (buffer-string)))
          (should-not (string-match-p ":state: draft" (buffer-string)))
          (td/command-workspace--agent-notebook-ingest-event
           (td/command-workspace-test--session-start-event))
          (should (equal td/command-workspace--agent-notebook-session-id
                         "sess-1"))
          (should (eq (gethash
                      "sess-1"
                      td/command-workspace--agent-notebook-session-buffers)
                      buffer)))))))

(ert-deftest td-command-workspace-agent-notebook-registers-code-cells-submit ()
  (let ((old-code-cells-eval-region-commands
         (and (boundp 'code-cells-eval-region-commands)
              code-cells-eval-region-commands))
        called)
    (unwind-protect
        (progn
          (setq code-cells-eval-region-commands '((text-mode . ignore)))
          (cl-letf (((symbol-function 'td/command-workspace-agent-notebook-submit)
                     (lambda (&optional prompt)
                       (setq called prompt)
                       'submitted)))
            (with-temp-buffer
              (td/command-workspace-agent-notebook-mode)
              (let ((entry
                     (assq 'td/command-workspace-agent-notebook-mode
                           code-cells-eval-region-commands)))
                (should entry)
                (should (eq (funcall (cdr entry) (point-min) (point-max))
                            'submitted))
                (should-not called)))))
      (setq code-cells-eval-region-commands
            old-code-cells-eval-region-commands))))

(ert-deftest td-command-workspace-agent-notebook-submits-visible-draft-without-markers ()
  (td/command-workspace-test--reset-agent-state)
  (let (run-call)
    (cl-letf (((symbol-function 'td/command-workspace--agent-run)
               (lambda (prompt project-root task-title buffer)
                 (setq run-call (list prompt project-root task-title buffer))
                 '("/tmp/bin/td-agent" "run"))))
      (with-temp-buffer
        (insert "# td-agent notebook: smoke test new\n"
                "# project: ~/Projects/dotfiles/\n"
                "# task_title: smoke test new\n"
                "\n"
                "# %% Draft Notebook Cell\n"
                "# :state: draft\n"
                "\n"
                "Hello\n")
        (td/command-workspace-agent-notebook-mode)
        (setq-local td/command-workspace--agent-notebook-project-root
                    "/tmp/dotfiles/")
        (setq-local td/command-workspace--agent-notebook-task-title
                    "smoke test new")
        (goto-char (point-min))
        (search-forward "Hello")
        (insert " world")
        (td/command-workspace--agent-notebook-clear-draft)
        (should (equal (td/command-workspace--agent-notebook-draft-prompt)
                       "Hello world"))
        (should (td/command-workspace--agent-notebook-draft-live-p))
        (td/command-workspace--agent-notebook-clear-draft)
        (td/command-workspace-agent-notebook-submit)
        (should (equal (nth 0 run-call) "Hello world"))
        (should (equal (nth 1 run-call) "/tmp/dotfiles/"))
        (should (equal (nth 2 run-call) "smoke test new"))
        (should (eq (nth 3 run-call) (current-buffer)))))))

(ert-deftest td-command-workspace-agent-notebook-self-inserts-g-in-draft ()
  (td/command-workspace-test--reset-agent-state)
  (td/command-workspace-test--with-notebook
   (td/command-workspace--agent-notebook-initialize
    (current-buffer)
    "/tmp/dotfiles/"
    "Smoke test"
    "")
   (goto-char td/command-workspace--agent-notebook-draft-begin-marker)
   (should (eq (key-binding (kbd "g")) #'self-insert-command))
   (should-not (get-text-property (point) 'read-only))
   (insert "ag")
   (should (equal (td/command-workspace--agent-notebook-draft-prompt)
                  "ag"))))

(ert-deftest td-command-workspace-agent-notebook-reconciles-from-canonical-log ()
  (td/command-workspace-test--reset-agent-state)
  (let ((log-file (make-temp-file "td-agent-events" nil ".jsonl"))
        opened-root
        popped-buffer)
    (unwind-protect
        (let ((td/command-workspace-agent-transcript-log log-file))
          (td/command-workspace-test--write-jsonl
           log-file
           (td/command-workspace-test--canonical-session-events))
          (cl-letf (((symbol-function 'td/command-workspace-open-project-workspace)
                     (lambda (&optional project-root _file)
                       (setq opened-root project-root)))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (buffer-or-name &rest _args)
                       (setq popped-buffer
                             (if (bufferp buffer-or-name)
                                 buffer-or-name
                               (get-buffer buffer-or-name)))
                       popped-buffer)))
            (let ((buffer
                   (td/command-workspace-new-agent-notebook
                    "Inspect workspace"
                    "/tmp/dotfiles/")))
              (with-current-buffer buffer
                (setq-local td/command-workspace--agent-notebook-session-id
                            "sess-1")
                (td/command-workspace-agent-notebook-refresh-transcript)
                (let ((text (buffer-string)))
                  (should (equal opened-root "/tmp/dotfiles/"))
                  (should (eq popped-buffer buffer))
                  (should (string-match-p "Inspect repository state" text))
                  (should (string-match-p "Canonical summary from event log" text))
                  (should (= 1 (td/command-workspace-test--count-matches
                                ":state: complete" text)))
                  (should (= 1 (td/command-workspace-test--count-matches
                                ":state: draft" text)))
                  (should (string-match-p "Tool Activity" text))
                  (should (string-match-p "bash" text))
                  (should (string-match-p "canonical tool output" text)))
                (goto-char (point-min))
                (search-forward "canonical tool output")
                (should (get-text-property (match-beginning 0) 'invisible))
                (goto-char (point-min))
                (search-forward "Inspect repository state")
                (should-error (insert "not editable") :type 'text-read-only)
                (goto-char (point-min))
                (search-forward "# %% Draft Notebook Cell")
                (goto-char td/command-workspace--agent-notebook-draft-begin-marker)
                (insert "Follow-up prompt")
                (should (string-match-p "Follow-up prompt"
                                        (buffer-string)))))))
      (delete-file log-file))))

(ert-deftest td-command-workspace-agent-notebook-submits-next-draft-with-resume ()
  (td/command-workspace-test--reset-agent-state)
  (let ((log-file (make-temp-file "td-agent-events" nil ".jsonl"))
        made-process
        process-put-call)
    (unwind-protect
        (let ((td/command-workspace-agent-transcript-log log-file))
          (td/command-workspace-test--write-jsonl
           log-file
           (td/command-workspace-test--canonical-session-events))
          (cl-letf (((symbol-function 'td/command-workspace--agent-executable)
                     (lambda () "/tmp/bin/td-agent"))
                    ((symbol-function 'make-process)
                     (lambda (&rest plist)
                       (setq made-process plist)
                       'fake-process))
                    ((symbol-function 'process-put)
                     (lambda (process property value)
                       (setq process-put-call (list process property value))
                       value)))
            (td/command-workspace-test--with-notebook
             (setq-local td/command-workspace--agent-notebook-session-id
                         "sess-1")
             (td/command-workspace-agent-notebook-refresh-transcript)
             (goto-char (point-min))
             (search-forward "# %% Draft Notebook Cell")
             (goto-char td/command-workspace--agent-notebook-draft-begin-marker)
             (insert "Continue with implementation")
             (td/command-workspace-agent-notebook-submit)
             (should
              (equal (plist-get made-process :command)
                     '("/tmp/bin/td-agent"
                       "resume"
                       "sess-1"
                       "Continue with implementation")))
             (should (equal process-put-call
                            '(fake-process td-agent-events nil)))
             (should (eq td/command-workspace--agent-notebook-process
                         'fake-process))
             (should (string-match-p ":state: running" (buffer-string)))
             (should-not (string-match-p ":state: draft"
                                         (buffer-string))))))
      (delete-file log-file))))

(ert-deftest td-command-workspace-agent-notebook-streams-output-and-osc-events ()
  (td/command-workspace-test--reset-agent-state)
  (let (process-events made-process)
    (cl-letf (((symbol-function 'td/command-workspace--agent-executable)
               (lambda () "/tmp/bin/td-agent"))
              ((symbol-function 'make-process)
               (lambda (&rest plist)
                 (setq made-process plist)
                 'fake-process))
              ((symbol-function 'process-buffer)
               (lambda (_process)
                 (current-buffer)))
              ((symbol-function 'process-get)
               (lambda (_process property)
                 (when (eq property 'td-agent-events)
                   process-events)))
              ((symbol-function 'process-put)
               (lambda (_process property value)
                 (when (eq property 'td-agent-events)
                   (setq process-events value))
                 value)))
      (td/command-workspace-test--with-notebook
       (td/command-workspace--agent-notebook-initialize
        (current-buffer)
        "/tmp/dotfiles/"
        "Inspect workspace"
        "Inspect repository state")
       (td/command-workspace-agent-notebook-submit)
       (should (equal (plist-get made-process :command)
                      '("/tmp/bin/td-agent"
                        "run"
                        "--title"
                        "Inspect workspace"
                        "Inspect repository state")))
       (td/command-workspace--agent-notebook-insert-process-output
        'fake-process
        (concat
         "live output\n"
         "\e]777;notify;warp://cli-agent;"
         (json-encode (td/command-workspace-test--session-start-event))
         "\a"))
       (should (string-match-p "live output" (buffer-string)))
       (should (equal td/command-workspace--agent-notebook-session-id
                      "sess-1"))
       (should (= 1 (length process-events)))))))

(ert-deftest td-command-workspace-agent-notebook-strips-fragmented-osc-events ()
  (td/command-workspace-test--reset-agent-state)
  (let (process-events)
    (cl-letf (((symbol-function 'process-buffer)
               (lambda (_process)
                 (current-buffer)))
              ((symbol-function 'process-get)
               (lambda (_process property)
                 (when (eq property 'td-agent-events)
                   process-events)))
              ((symbol-function 'process-put)
               (lambda (_process property value)
                 (when (eq property 'td-agent-events)
                   (setq process-events value))
                 value)))
      (td/command-workspace-test--with-notebook
       (td/command-workspace--agent-notebook-open-output-block)
       (let* ((prefix td/command-workspace--agent-osc-prefix)
              (event (json-encode
                      (td/command-workspace-test--session-start-event))))
         (td/command-workspace--agent-notebook-insert-process-output
          'fake-process
          (substring prefix 0 12))
         (td/command-workspace--agent-notebook-insert-process-output
          'fake-process
          (concat (substring prefix 12) event "\a"))
         (let ((text (buffer-string)))
           (should-not (string-match-p "warp://cli-agent" text))
           (should-not (string-match-p "\"session_start\"" text)))
         (should (equal td/command-workspace--agent-notebook-session-id
                        "sess-1"))
         (should (= 1 (length process-events))))))))

(ert-deftest td-command-workspace-agent-notebook-strips-fragmented-osc-body ()
  (td/command-workspace-test--reset-agent-state)
  (let (process-events)
    (cl-letf (((symbol-function 'process-buffer)
               (lambda (_process)
                 (current-buffer)))
              ((symbol-function 'process-get)
               (lambda (_process property)
                 (when (eq property 'td-agent-events)
                   process-events)))
              ((symbol-function 'process-put)
               (lambda (_process property value)
                 (when (eq property 'td-agent-events)
                   (setq process-events value))
                 value)))
      (td/command-workspace-test--with-notebook
       (td/command-workspace--agent-notebook-open-output-block)
       (let* ((event
               (json-encode
                '(("event" . "session_start")
                  ("session_id" . "sess-1")
                  ("project" . "dotfiles")
                  ("cwd" . "/tmp/dotfiles/")
                  ("project_context" .
                   (("cwd" . "/tmp/dotfiles/")
                    ("project" . "dotfiles"))))))
              (split-at (/ (length event) 2)))
         (td/command-workspace--agent-notebook-insert-process-output
          'fake-process
          (concat "before\n"
                  td/command-workspace--agent-osc-prefix
                  (substring event 0 split-at)))
         (let ((text (buffer-string)))
           (should (string-match-p "before" text))
           (should-not (string-match-p "project_context" text))
           (should-not (string-match-p "warp://cli-agent" text)))
         (should td/command-workspace--agent-notebook-osc-active-p)
         (should-not process-events)
         (td/command-workspace--agent-notebook-insert-process-output
          'fake-process
          (concat (substring event split-at) "\aafter\n"))
         (let ((text (buffer-string)))
           (should (string-match-p "before" text))
           (should (string-match-p "after" text))
           (should-not (string-match-p "project_context" text))
           (should-not (string-match-p "warp://cli-agent" text)))
         (should-not td/command-workspace--agent-notebook-osc-active-p)
         (should (equal td/command-workspace--agent-notebook-session-id
                        "sess-1"))
         (should (= 1 (length process-events))))))))

(ert-deftest td-command-workspace-agent-notebook-sentinel-hides-success-debug ()
  (td/command-workspace-test--reset-agent-state)
  (cl-letf (((symbol-function 'process-buffer)
             (lambda (_process)
               (current-buffer)))
            ((symbol-function 'process-exit-status)
             (lambda (_process)
               0))
            ((symbol-function 'process-get)
             (lambda (_process property)
               (when (eq property 'td-agent-events)
                 (list (td/command-workspace-test--session-start-event))))))
    (td/command-workspace-test--with-notebook
     (td/command-workspace--agent-notebook-open-output-block)
     (td/command-workspace--agent-notebook-sentinel 'fake-process "finished\n")
     (let ((text (buffer-string)))
       (should-not (string-match-p "\\* Events" text))
       (should-not (string-match-p "td-agent exited 0" text))
       (should-not (string-match-p "session_start" text))))))

(ert-deftest td-command-workspace-agent-notebook-reconciles-when-cell-stops ()
  (td/command-workspace-test--reset-agent-state)
  (let ((log-file (make-temp-file "td-agent-events" nil ".jsonl")))
    (unwind-protect
        (let ((td/command-workspace-agent-transcript-log log-file)
              (td/command-workspace-agent-notification-popup-function nil))
          (td/command-workspace-test--write-jsonl
           log-file
           (td/command-workspace-test--canonical-session-events))
          (td/command-workspace-test--with-notebook
           (td/command-workspace--agent-notebook-initialize
            (current-buffer)
            "/tmp/dotfiles/"
            "Inspect workspace"
            "Inspect repository state")
           (setq-local td/command-workspace--agent-notebook-session-id "sess-1")
           (td/command-workspace--agent-notebook-mark-draft-running)
           (td/command-workspace--agent-notebook-open-output-block)
           (td/command-workspace--agent-notebook-insert "streaming glitch")
           (td/command-workspace--agent-notebook-ingest-event
            '(("event" . "stop")
              ("session_id" . "sess-1")
              ("summary" . "streaming glitch")
              ("project" . "dotfiles")
              ("cwd" . "/tmp/dotfiles/")))
           (let ((text (buffer-string)))
             (should (string-match-p "Canonical summary from event log" text))
             (should-not (string-match-p "streaming glitch" text))
             (should (= 1 (td/command-workspace-test--count-matches
                           ":state: complete" text)))
             (should (= 1 (td/command-workspace-test--count-matches
                           ":state: draft" text))))))
      (delete-file log-file))))

(ert-deftest td-command-workspace-permission-request-ingestion-blocks-cell ()
  (td/command-workspace-test--reset-agent-state)
  (td/command-workspace-test--with-notebook
   (let ((inhibit-read-only t))
     (insert "# %% Running Notebook Cell\n# :state: running\n\nOutput:\n\n```text\nbefore"))
   (setq-local td/command-workspace--agent-notebook-output-block-open-p t)
   (should
    (eq (td/command-workspace--agent-notebook-ingest-event
         (td/command-workspace-test--permission-request-event))
        'permission-request))
   (should
    (equal td/command-workspace--agent-notebook-blocked-permission-request-id
           "req-1"))
   (should-not td/command-workspace--agent-notebook-output-block-open-p)
   (should (equal td/command-workspace--agent-notebook-session-id "sess-1"))
   (should (gethash "req-1" td/command-workspace--agent-permission-requests))
   (let ((text (buffer-string)))
     (should (string-match-p "# %% Blocked Permission Request" text))
     (should (string-match-p ":state: blocked" text))
     (should (string-match-p "Tool: bash" text))
     (should (string-match-p "Session: sess-1" text))
     (should (string-match-p "Project: dotfiles" text))
     (should (string-match-p "Input preview:" text))
     (should (string-match-p "make test" text)))))

(ert-deftest td-command-workspace-question-asked-ingestion-blocks-cell ()
  (td/command-workspace-test--reset-agent-state)
  (td/command-workspace-test--with-notebook
   (let ((inhibit-read-only t))
     (insert "# %% Running Notebook Cell\n# :state: running\n\nOutput:\n\n```text\nbefore"))
   (setq-local td/command-workspace--agent-notebook-output-block-open-p t)
   (should
    (eq (td/command-workspace--agent-notebook-ingest-event
         (td/command-workspace-test--question-asked-event))
        'question-asked))
   (should
    (equal td/command-workspace--agent-notebook-blocked-question-id
           "q-1"))
   (should-not td/command-workspace--agent-notebook-blocked-permission-request-id)
   (should-not td/command-workspace--agent-notebook-output-block-open-p)
   (should (equal td/command-workspace--agent-notebook-session-id "sess-1"))
   (should (gethash "q-1" td/command-workspace--agent-questions))
   (should
    (equal td/command-workspace--agent-action-event
           (td/command-workspace-test--question-asked-event)))
   (let ((text (buffer-string)))
     (should (string-match-p "# %% Blocked Question" text))
     (should (string-match-p ":state: blocked" text))
     (should (string-match-p ":question_id: q-1" text))
     (should (string-match-p "Question: Which migration path should I use\\?" text))
     (should (string-match-p "Session: sess-1" text))
     (should (string-match-p "Project: dotfiles" text))
     (should (string-match-p "Context:" text))
     (should (string-match-p "Two data migrations are possible" text)))))

(ert-deftest td-command-workspace-permission-replied-updates-and-resumes-output ()
  (td/command-workspace-test--reset-agent-state)
  (td/command-workspace-test--with-notebook
   (let ((inhibit-read-only t))
     (insert "# %% Running Notebook Cell\n# :state: running\n\nOutput:\n\n```text\nbefore"))
   (setq-local td/command-workspace--agent-notebook-output-block-open-p t)
   (td/command-workspace--agent-notebook-ingest-event
    (td/command-workspace-test--permission-request-event))
   (should
    (eq (td/command-workspace--agent-notebook-ingest-event
         (td/command-workspace-test--permission-replied-event "deny"))
        'permission-replied))
   (should-not td/command-workspace--agent-notebook-blocked-permission-request-id)
   (should td/command-workspace--agent-notebook-output-block-open-p)
   (should
    (eq (plist-get
         (gethash "req-1" td/command-workspace--agent-permission-requests)
         :state)
        'replied))
   (td/command-workspace--agent-notebook-insert "after reply")
   (let ((text (buffer-string)))
     (should (string-match-p "# %% Permission Replied" text))
     (should (string-match-p ":state: running" text))
     (should (string-match-p ":decision: deny" text))
     (should (string-match-p "Output Continued:" text))
     (should (string-match-p "after reply" text)))))

(ert-deftest td-command-workspace-agent-action-queue-lists-agent-states ()
  (td/command-workspace-test--reset-agent-state)
  (let ((td/command-workspace-agent-notification-popup-function nil))
    (td/command-workspace-test--with-notebook
     (td/command-workspace--agent-notebook-ingest-event
      (td/command-workspace-test--permission-request-event))
     (td/command-workspace--agent-notebook-ingest-event
      (td/command-workspace-test--question-asked-event))
     (td/command-workspace--agent-notebook-ingest-event
      (td/command-workspace-test--stop-event))
     (td/command-workspace--agent-notebook-ingest-event
      (td/command-workspace-test--session-error-event))))
  (let ((items (td/command-workspace--agent-action-queue-items)))
    (should (seq-find (lambda (item)
                        (eq (plist-get item :kind) 'permission))
                      items))
    (should (seq-find (lambda (item)
                        (eq (plist-get item :kind) 'question))
                      items))
    (should (seq-find (lambda (item)
                        (eq (plist-get item :kind) 'completed))
                      items))
    (should (seq-find (lambda (item)
                        (eq (plist-get item :kind) 'error))
                      items))))

(ert-deftest td-command-workspace-agent-action-queue-filters-and-read-state ()
  (td/command-workspace-test--reset-agent-state)
  (let ((td/command-workspace-agent-notification-popup-function nil))
    (td/command-workspace-test--with-notebook
     (td/command-workspace--agent-notebook-ingest-event
      (td/command-workspace-test--permission-request-event))
     (td/command-workspace--agent-notebook-ingest-event
      (td/command-workspace-test--question-asked-event))
     (td/command-workspace--agent-notebook-ingest-event
      (td/command-workspace-test--stop-event))
     (td/command-workspace--agent-notebook-ingest-event
      (td/command-workspace-test--session-error-event))))
  (should (= (length (td/command-workspace--agent-action-queue-items
                      'actionable))
             2))
  (should (= (length (td/command-workspace--agent-action-queue-items
                      'completed))
             1))
  (should (= (length (td/command-workspace--agent-action-queue-items
                      'unread))
             4))
  (td/command-workspace--agent-notifications-mark-session-read "sess-1")
  (should (= (length (td/command-workspace--agent-action-queue-items
                      'unread))
             0))
  (should (= (length (td/command-workspace--agent-action-queue-items
                      'actionable))
             2))
  (should (= (length (td/command-workspace--agent-action-queue-items))
             2)))

(ert-deftest td-command-workspace-agent-action-queue-activation-opens-transient ()
  (td/command-workspace-test--reset-agent-state)
  (let ((td/command-workspace-agent-notification-popup-function nil))
    (td/command-workspace-test--with-notebook
     (td/command-workspace--agent-notebook-ingest-event
      (td/command-workspace-test--permission-request-event))))
  (let* ((item
          (seq-find (lambda (item)
                      (eq (plist-get item :kind) 'permission))
                    (td/command-workspace--agent-action-queue-items)))
         (notification
          (plist-get item :notification))
         opened-root
         popped-buffer
         transient-event)
    (cl-letf (((symbol-function 'td/command-workspace-open-project-workspace)
               (lambda (&optional project-root _file)
                 (setq opened-root project-root)))
              ((symbol-function 'pop-to-buffer)
               (lambda (buffer-or-name &rest _args)
                 (setq popped-buffer
                       (if (bufferp buffer-or-name)
                           buffer-or-name
                         (get-buffer buffer-or-name)))))
              ((symbol-function 'td/command-workspace-agent-action-transient)
               (lambda (event)
                 (setq transient-event event)
                 'transient-opened)))
      (should
       (eq (plist-get
            (td/command-workspace--agent-action-queue-activate-item item)
            :kind)
           'permission))
      (should (equal opened-root "/tmp/dotfiles/"))
      (should (buffer-live-p popped-buffer))
      (should (equal transient-event
                     (td/command-workspace-test--permission-request-event)))
      (should
       (eq (plist-get
            (gethash (plist-get notification :id)
                     td/command-workspace--agent-notifications)
            :state)
           'read)))))

(ert-deftest td-command-workspace-agent-notification-activation-opens-action ()
  (td/command-workspace-test--reset-agent-state)
  (let ((td/command-workspace-agent-notification-popup-function nil))
    (td/command-workspace-test--with-notebook
     (td/command-workspace--agent-notebook-ingest-event
      (td/command-workspace-test--question-asked-event))))
  (let* ((notification-id
          (plist-get
           (seq-find (lambda (item)
                       (eq (plist-get item :kind) 'question))
                     (td/command-workspace--agent-action-queue-items))
           :id))
         opened-root
         popped-buffer
         transient-event)
    (cl-letf (((symbol-function 'td/command-workspace-open-project-workspace)
               (lambda (&optional project-root _file)
                 (setq opened-root project-root)))
              ((symbol-function 'pop-to-buffer)
               (lambda (buffer-or-name &rest _args)
                 (setq popped-buffer
                       (if (bufferp buffer-or-name)
                           buffer-or-name
                         (get-buffer buffer-or-name)))))
              ((symbol-function 'td/command-workspace-agent-action-transient)
               (lambda (event)
                 (setq transient-event event)
                 'transient-opened)))
      (td/command-workspace-agent-notification-activate notification-id)
      (should (equal opened-root "/tmp/dotfiles/"))
      (should (buffer-live-p popped-buffer))
      (should (equal transient-event
                     (td/command-workspace-test--question-asked-event))))))

(ert-deftest td-command-workspace-agent-notification-fallback-uses-message-and-queue ()
  (td/command-workspace-test--reset-agent-state)
  (let ((td/command-workspace-agent-notification-popup-function nil)
        messages)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (let ((notification
             (td/command-workspace--agent-notification-record
              (td/command-workspace-test--stop-event))))
        (should (eq (plist-get notification :presented-as) 'message))
        (should (string-match-p "td-agent: "
                                (car messages)))
        (should
         (seq-find (lambda (item)
                     (eq (plist-get item :kind) 'completed))
                   (td/command-workspace--agent-action-queue-items)))))))

(ert-deftest td-command-workspace-agent-notification-does-not-use-desktop ()
  (td/command-workspace-test--reset-agent-state)
  (let ((td/command-workspace-agent-notification-popup-function nil)
        desktop-called)
    (cl-letf (((symbol-function 'notifications-notify)
               (lambda (&rest _args)
                 (setq desktop-called t))))
      (td/command-workspace--agent-notification-record
       (td/command-workspace-test--permission-request-event))
      (should-not desktop-called))))

(ert-deftest td-command-workspace-opening-session-marks-notifications-read ()
  (td/command-workspace-test--reset-agent-state)
  (let* ((td/command-workspace-agent-notification-popup-function nil)
         (event (td/command-workspace-test--stop-event))
         (notification
          (td/command-workspace--agent-notification-record event))
         opened-root
         popped-buffer)
    (cl-letf (((symbol-function 'td/command-workspace-open-project-workspace)
               (lambda (&optional project-root _file)
                 (setq opened-root project-root)))
              ((symbol-function 'pop-to-buffer)
               (lambda (buffer-or-name &rest _args)
                 (setq popped-buffer
                       (if (bufferp buffer-or-name)
                           buffer-or-name
                         (get-buffer buffer-or-name))))))
      (td/command-workspace--agent-action-open-notebook-buffer event)
      (should (equal opened-root "/tmp/dotfiles/"))
      (should (buffer-live-p popped-buffer))
      (should
       (eq (plist-get
            (gethash (plist-get notification :id)
                     td/command-workspace--agent-notifications)
            :state)
           'read))
      (should-not (td/command-workspace--agent-action-queue-items)))))

(ert-deftest td-command-workspace-agent-action-description-shows-permission-context ()
  (let* ((event (td/command-workspace-test--permission-request-event))
         (description
          (let ((td/command-workspace--agent-action-event event))
            (td/command-workspace--agent-action-transient-description))))
    (should (string-match-p "Tool: bash" description))
    (should (string-match-p "Request: req-1" description))
    (should (string-match-p "Session: sess-1" description))
    (should (string-match-p "Project: dotfiles" description))
     (should (string-match-p "Input preview:" description))
     (should (string-match-p "make test" description))))

(ert-deftest td-command-workspace-agent-action-description-shows-question-context ()
  (let* ((event (td/command-workspace-test--question-asked-event))
         (description
          (let ((td/command-workspace--agent-action-event event))
            (td/command-workspace--agent-action-transient-description))))
    (should (string-match-p "Question: Which migration path should I use\\?" description))
    (should (string-match-p "Question ID: q-1" description))
    (should (string-match-p "Session: sess-1" description))
    (should (string-match-p "Project: dotfiles" description))
    (should (string-match-p "Context:" description))
    (should (string-match-p "Two data migrations are possible" description))))

(ert-deftest td-command-workspace-permission-reply-builds-cli-argv ()
  (cl-letf (((symbol-function 'td/command-workspace--agent-executable)
             (lambda () "/tmp/bin/td-agent")))
    (let ((td/command-workspace-agent-permission-reply-command
           '("permission" "reply")))
      (should
       (equal
        (td/command-workspace--agent-permission-reply-argv
         (td/command-workspace-test--permission-request-event)
         "approve")
        '("/tmp/bin/td-agent" "permission" "reply" "req-1" "approve")))
      (should
       (equal
        (td/command-workspace--agent-permission-reply-argv
         (td/command-workspace-test--permission-request-event)
         "deny")
        '("/tmp/bin/td-agent" "permission" "reply" "req-1" "deny"))))))

(ert-deftest td-command-workspace-agent-resume-builds-cli-argv ()
  (cl-letf (((symbol-function 'td/command-workspace--agent-executable)
             (lambda () "/tmp/bin/td-agent")))
    (let ((td/command-workspace-agent-resume-command "resume"))
      (should
       (equal
        (td/command-workspace--agent-resume-argv
         (td/command-workspace-test--question-asked-event)
         "Use the reversible migration.")
        '("/tmp/bin/td-agent"
          "resume"
          "sess-1"
          "Use the reversible migration."))))))

(ert-deftest td-command-workspace-permission-actions-dispatch-decisions ()
  (let ((td/command-workspace--agent-action-event
         (td/command-workspace-test--permission-request-event))
        calls)
    (cl-letf (((symbol-function 'td/command-workspace--agent-permission-reply)
               (lambda (event decision)
                 (push (list (td/command-workspace--agent-permission-request-id
                              event)
                             decision)
                       calls)
                 decision)))
      (should (equal (td/command-workspace-agent-action-approve) "approve"))
      (should (equal (td/command-workspace-agent-action-deny) "deny"))
      (should
       (equal (nreverse calls)
              '(("req-1" "approve")
                ("req-1" "deny")))))))

(ert-deftest td-command-workspace-question-answer-dispatches-agent-resume ()
  (let ((td/command-workspace--agent-action-event
         (td/command-workspace-test--question-asked-event))
        calls
        permission-reply-called)
    (cl-letf (((symbol-function 'td/command-workspace--agent-action-open-notebook-buffer)
               (lambda (_event)
                 'notebook-buffer))
              ((symbol-function 'td/command-workspace--agent-resume)
               (lambda (event prompt &optional buffer)
                 (push (list (td/command-workspace--agent-event-session-id event)
                             prompt
                             buffer)
                       calls)
                 'resume-started))
              ((symbol-function 'td/command-workspace--agent-permission-reply)
               (lambda (&rest _args)
                 (setq permission-reply-called t)
                 'permission-reply)))
      (should
       (eq (td/command-workspace-agent-action-answer
            "Use the reversible migration.")
           'resume-started))
      (should
       (equal (nreverse calls)
              '(("sess-1" "Use the reversible migration." notebook-buffer))))
      (should-not permission-reply-called))))

(ert-deftest td-command-workspace-question-does-not-dispatch-permission-reply ()
  (let ((td/command-workspace--agent-action-event
         (td/command-workspace-test--question-asked-event))
        permission-reply-called)
    (cl-letf (((symbol-function 'td/command-workspace--agent-permission-reply)
               (lambda (&rest _args)
                 (setq permission-reply-called t))))
      (should-error (td/command-workspace-agent-action-approve)
                    :type 'user-error)
      (should-error (td/command-workspace-agent-action-deny)
                    :type 'user-error)
      (should-not permission-reply-called))))

(ert-deftest td-command-workspace-permission-reply-invokes-cli-command ()
  (let ((made-process nil))
    (cl-letf (((symbol-function 'td/command-workspace--agent-executable)
               (lambda () "/tmp/bin/td-agent"))
              ((symbol-function 'make-process)
               (lambda (&rest plist)
                 (setq made-process plist)
                 'fake-process)))
      (should
       (equal
        (td/command-workspace--agent-permission-reply
         (td/command-workspace-test--permission-request-event)
         "approve")
        '("/tmp/bin/td-agent" "permission" "reply" "req-1" "approve")))
      (should
       (equal (plist-get made-process :command)
              '("/tmp/bin/td-agent" "permission" "reply" "req-1" "approve"))))))

(ert-deftest td-command-workspace-agent-resume-invokes-cli-command-and-renders-cell ()
  (let ((made-process nil)
        (process-put-call nil))
    (cl-letf (((symbol-function 'td/command-workspace--agent-executable)
               (lambda () "/tmp/bin/td-agent"))
              ((symbol-function 'make-process)
               (lambda (&rest plist)
                 (setq made-process plist)
                 'fake-process))
              ((symbol-function 'process-put)
               (lambda (process property value)
                 (setq process-put-call (list process property value))
                 value)))
      (td/command-workspace-test--with-notebook
       (setq-local td/command-workspace--agent-notebook-blocked-question-id "q-1")
       (should
        (equal
         (td/command-workspace--agent-resume
          (td/command-workspace-test--question-asked-event)
          "Use the reversible migration."
          (current-buffer))
         '("/tmp/bin/td-agent"
           "resume"
           "sess-1"
           "Use the reversible migration.")))
       (should
        (equal (plist-get made-process :command)
               '("/tmp/bin/td-agent"
                 "resume"
                 "sess-1"
                 "Use the reversible migration.")))
       (should (eq td/command-workspace--agent-notebook-process 'fake-process))
       (should td/command-workspace--agent-notebook-output-block-open-p)
       (should-not td/command-workspace--agent-notebook-blocked-question-id)
       (should (equal process-put-call
                      '(fake-process td-agent-events nil)))
       (let ((text (buffer-string)))
         (should (string-match-p "# %% Running Notebook Cell" text))
         (should (string-match-p ":state: running" text))
         (should (string-match-p ":session: sess-1" text))
         (should (string-match-p "Use the reversible migration" text))
         (should (string-match-p "Output:" text)))))))

(ert-deftest td-command-workspace-agent-action-open-notebook-navigates-project-workspace ()
  (let ((td/command-workspace--agent-action-event
         (td/command-workspace-test--question-asked-event))
        (opened-root nil)
        (popped-buffer nil))
    (cl-letf (((symbol-function 'td/command-workspace-open-project-workspace)
               (lambda (&optional project-root _file)
                 (setq opened-root project-root)))
              ((symbol-function 'pop-to-buffer)
               (lambda (buffer-or-name &rest _args)
                 (setq popped-buffer
                       (if (bufferp buffer-or-name)
                           buffer-or-name
                         (get-buffer buffer-or-name)))
                 popped-buffer)))
      (let ((buffer (td/command-workspace-agent-action-open-notebook)))
        (should (buffer-live-p buffer))
        (should (equal opened-root "/tmp/dotfiles/"))
        (should (eq popped-buffer buffer))))))

(ert-deftest td-command-workspace-permission-review-dismiss-opens-notebook ()
  (let ((td/command-workspace--agent-action-event
         (td/command-workspace-test--permission-request-event))
        opened-root
        popped-buffer)
    (cl-letf (((symbol-function 'td/command-workspace-open-project-workspace)
               (lambda (&optional project-root)
                 (setq opened-root project-root)
                 'opened))
              ((symbol-function 'pop-to-buffer)
               (lambda (buffer-or-name &rest _args)
                 (setq popped-buffer
                       (if (bufferp buffer-or-name)
                           buffer-or-name
                         (get-buffer buffer-or-name)))
                 popped-buffer)))
      (should (eq (td/command-workspace-agent-action-review-dismiss)
                  'dismissed))
      (should (equal opened-root "/tmp/dotfiles/"))
      (should (buffer-live-p popped-buffer)))))

(provide 'td-command-workspace-tests)
;;; td-command-workspace-tests.el ends here
