;;; td-command-workspace.el --- Project workspaces + tmux-backed terminals -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'tab-bar)
(require 'tabulated-list)
(require 'json)
(require 'org)
(require 'transient)
(require 'code-cells nil t)

(defvar eat-buffer-name)
(defvar code-cells-eval-region-commands)
(declare-function code-cells-mode "code-cells")

(defgroup td/command-workspace nil
  "Project workspace and durable terminal controls."
  :group 'tools)

(defcustom td/command-workspace-terminal-backend 'eat
  "Preferred terminal backend.

Prefer `eat'. If unavailable, fallback to `vterm`."
  :type '(choice (const :tag "EAT" eat)
                 (const :tag "vterm" vterm))
  :group 'td/command-workspace)

(defcustom td/command-workspace-session-hash-length 6
  "Number of hash chars appended to generated project session names."
  :type 'natnum
  :group 'td/command-workspace)

(defcustom td/command-workspace-session-prefix "cmdws"
  "Prefix for tmux session names and terminal buffers."
  :type 'string
  :group 'td/command-workspace)

(defcustom td/command-workspace-agent-source-directory
  (expand-file-name "~/Projects/personal/td-agent/")
  "Source directory for the local Workflow Agent CLI."
  :type 'directory
  :group 'td/command-workspace)

(defcustom td/command-workspace-agent-executable nil
  "Executable path for the local Workflow Agent CLI.

When nil, prefer the built binary under
`td/command-workspace-agent-source-directory', then `td-agent' from PATH."
  :type '(choice (const :tag "Auto" nil)
                 file)
  :group 'td/command-workspace)

(defcustom td/command-workspace-agent-transcript-log
  (expand-file-name "~/.td-agent/transcripts/events.jsonl")
  "Canonical JSONL transcript log written by the Workflow Agent CLI."
  :type 'file
  :group 'td/command-workspace)

(defcustom td/command-workspace-agent-permission-reply-command '("permission" "reply")
  "Workflow Agent CLI command parts that reply to pending permission requests."
  :type '(repeat string)
  :group 'td/command-workspace)

(defcustom td/command-workspace-agent-resume-command "resume"
  "Workflow Agent CLI subcommand that resumes an existing Transcript Session."
  :type 'string
  :group 'td/command-workspace)

(defcustom td/command-workspace-agent-run-command "run"
  "Workflow Agent CLI subcommand that starts a new Agent Task."
  :type 'string
  :group 'td/command-workspace)

(defcustom td/command-workspace-agent-auto-open-action-transient t
  "When non-nil, open the Agent Action Transient for live agent actions."
  :type 'boolean
  :group 'td/command-workspace)

(defcustom td/command-workspace-agent-notification-popup-function
  #'td/command-workspace--agent-notification-display-popup
  "Function used to show Emacs-local Agent Notifications.

The function receives one notification plist and should return non-nil when it
displayed an Emacs-local popup.  When nil or when it returns nil, notifications
fall back to `message' while remaining available in the Agent Action Queue."
  :type '(choice (const :tag "Fallback to message" nil)
                 function)
  :group 'td/command-workspace)

(defcustom td/command-workspace-agent-notification-popup-buffer-name
  "*td-agent notification*"
  "Buffer name used for minimal Emacs-local Agent Notification popups."
  :type 'string
  :group 'td/command-workspace)

(defvar td/command-workspace--session-buffers (make-hash-table :test #'equal))
(defvar td/command-workspace--agent-notebook-session-buffers
  (make-hash-table :test #'equal)
  "Notebook buffers keyed by Workflow Agent CLI session id.")
(defvar td/command-workspace--agent-permission-requests
  (make-hash-table :test #'equal)
  "Permission request records keyed by request id.")
(defvar td/command-workspace--agent-questions
  (make-hash-table :test #'equal)
  "Question records keyed by question id.")
(defvar td/command-workspace--agent-notifications
  (make-hash-table :test #'equal)
  "Agent Notification records keyed by notification id.")
(defvar td/command-workspace--agent-notification-order nil
  "Notification ids ordered by creation time, oldest first.")
(defvar td/command-workspace--agent-action-event nil
  "CLI Agent Event currently shown by the Agent Action Transient.")
(defvar-local td/command-workspace--has-attached-p nil)
(defvar-local td/command-workspace--agent-notebook-project-root nil)
(defvar-local td/command-workspace--agent-notebook-process nil)
(defvar-local td/command-workspace--agent-notebook-osc-fragment "")
(defvar-local td/command-workspace--agent-notebook-osc-active-p nil)
(defvar-local td/command-workspace--agent-notebook-output-block-open-p nil)
(defvar-local td/command-workspace--agent-notebook-session-id nil)
(defvar-local td/command-workspace--agent-notebook-task-title nil)
(defvar-local td/command-workspace--agent-notebook-draft-begin-marker nil)
(defvar-local td/command-workspace--agent-notebook-draft-end-marker nil)
(defvar-local td/command-workspace--agent-notebook-blocked-permission-request-id nil)
(defvar-local td/command-workspace--agent-notebook-blocked-question-id nil)
(defvar-local td/command-workspace--agent-notification-current-id nil)
(defvar-local td/command-workspace--agent-action-queue-filter nil)
(declare-function vterm "vterm" ())
(declare-function vterm-send-string "vterm" (string))
(declare-function eat "eat" (command))

(defvar td/command-workspace-prefix-map (make-sparse-keymap)
  "Prefix map for command workspace commands.")

(defun td/command-workspace--current-project-root ()
  "Return the current project root, or nil outside a project."
  (when-let* ((project (project-current)))
    (file-name-as-directory (project-root project))))

(defun td/command-workspace--current-root ()
  "Return a workspace root for the current buffer.

Prefer the current project root. Outside a project, use the current file's
directory, then `default-directory'."
  (or (td/command-workspace--current-project-root)
      (when-let* ((file (buffer-file-name)))
        (file-name-as-directory (file-name-directory file)))
      (when (and default-directory (file-directory-p default-directory))
        (file-name-as-directory default-directory))
      (user-error "No project or directory at point")))

(defun td/command-workspace--current-dashboard-file (workspace-root)
  "Return the current file as a dashboard target under WORKSPACE-ROOT.

This is used for non-project workspaces so opening a workspace from a loose file
such as an Org inbox returns to that file instead of a plain Dired buffer."
  (when-let* ((file (buffer-file-name)))
    (when (and (not (td/command-workspace--current-project-root))
               (file-in-directory-p file workspace-root))
      file)))

(defun td/command-workspace--session-name (project-root)
  "Return a stable tmux session name for PROJECT-ROOT."
  (let* ((base (or (and (not (string-empty-p (file-name-nondirectory
                                            (directory-file-name project-root))))
                   (file-name-nondirectory (directory-file-name project-root)))
                  "root"))
        (safe-base (replace-regexp-in-string "[^A-Za-z0-9._-]" "-" base))
        (hash (substring (secure-hash 'sha1 (file-truename project-root))
                         0 (max 4 td/command-workspace-session-hash-length))))
    (format "%s-%s-%s" td/command-workspace-session-prefix safe-base hash)))

(defun td/command-workspace--buffer-name (session-name)
  "Terminal buffer name for SESSION-NAME."
  (format "*%s*" session-name))

(defun td/command-workspace--tab-name (project-root)
  "Tab name for PROJECT-ROOT."
  (td/command-workspace--session-name project-root))

(defun td/command-workspace--tab-exists-p (tab-name)
  "Return non-nil when a tab with TAB-NAME already exists."
  (seq-some (lambda (tab)
              (string-equal tab-name (alist-get 'name tab)))
            (tab-bar-tabs)))

(defun td/command-workspace--command-tabs ()
  "Return tab names created by command-workspace."
  (let ((prefix (format "%s-" td/command-workspace-session-prefix)))
    (seq-filter
     (lambda (name)
       (string-prefix-p prefix name))
     (mapcar (lambda (tab) (alist-get 'name tab))
             (tab-bar-tabs)))))

(defun td/command-workspace--tmux-sessions ()
  "Return current tmux session names, if tmux is available."
  (when (td/command-workspace--tmux-available-p)
    (with-temp-buffer
      (let ((status (process-file "tmux" nil (current-buffer) nil
                                 "list-sessions" "-F" "#S")))
        (when (eq status 0)
          (split-string (string-trim (buffer-string)) "\n" t))))))

(defun td/command-workspace--status-candidates ()
  "Return completion candidates for workspace status."
  (let ((sessions (td/command-workspace--tmux-sessions))
        (tabs (td/command-workspace--command-tabs)))
    (mapcar (lambda (tab)
              (let ((has-session (member tab sessions)))
                (cons (format "%s  (%s)"
                              tab
                              (if has-session
                                  "tmux running"
                                "no tmux"))
                      tab)))
            tabs)))

(defun td/command-workspace-status ()
  "Open a command workspace through a tab/terminal status selector."
  (interactive)
  (let* ((candidates (td/command-workspace--status-candidates)))
    (unless candidates
      (user-error "No command-workspace tabs yet. Run `C-l w w` first."))
    (tab-bar-switch-to-tab
     (cdr (assoc (completing-read "Workspaces: " candidates nil t)
                 candidates)))))

(defun td/command-workspace--tmux-available-p ()
  "Return non-nil when tmux is available."
  (executable-find "tmux"))

(defun td/command-workspace--tmux-run (args)
  "Run `tmux' with ARGS and signal error on non-zero status."
  (unless (td/command-workspace--tmux-available-p)
    (user-error "tmux is required for project terminals"))
  (with-temp-buffer
    (let ((status (apply #'process-file "tmux" nil t nil args)))
      (unless (eq status 0)
        (user-error "tmux error while running: %s" (string-trim (buffer-string))))
      (string-trim (buffer-string)))))

(defun td/command-workspace--agent-built-executable ()
  "Return the source-tree built agent executable path."
  (expand-file-name "_build/default/bin/main.exe"
                    td/command-workspace-agent-source-directory))

(defun td/command-workspace--agent-executable ()
  "Return an executable Workflow Agent CLI path or signal an actionable error."
  (or (and td/command-workspace-agent-executable
           (file-executable-p td/command-workspace-agent-executable)
           td/command-workspace-agent-executable)
      (let ((built (td/command-workspace--agent-built-executable)))
        (when (file-executable-p built)
          built))
      (executable-find "td-agent")
      (user-error
       "td-agent is not built. Run `opam exec -- dune build @all` in %s"
       (abbreviate-file-name td/command-workspace-agent-source-directory))))

(defun td/command-workspace--agent-command (&rest args)
  "Return a shell command invoking td-agent with ARGS."
  (string-join
   (mapcar #'shell-quote-argument
           (cons (td/command-workspace--agent-executable) args))
   " "))

(defun td/command-workspace--tmux-new-window (session-name project-root window-name command)
  "Create and select a tmux window running COMMAND."
  (td/command-workspace--ensure-tmux-session session-name project-root)
  (let ((window-id
         (td/command-workspace--tmux-run
          (list "new-window"
                "-P"
                "-F" "#{window_id}"
                "-t" session-name
                "-n" window-name
                "-c" (file-name-as-directory project-root)
                command))))
    (td/command-workspace--tmux-run (list "select-window" "-t" window-id))
    window-id))

(defun td/command-workspace--tmux-agent-command (agent-args)
  "Return a shell command for an agent tmux window."
  (format "%s; td_agent_status=$?; printf '\\n[td-agent exited %%s]\\n' \"$td_agent_status\"; exec \"${SHELL:-/bin/zsh}\" -l"
          (apply #'td/command-workspace--agent-command agent-args)))

(defun td/command-workspace--tmux-session-exists-p (session-name)
  "Return non-nil when a tmux session named SESSION-NAME exists."
  (eq 0 (process-file "tmux" nil nil nil "has-session" "-t" session-name)))

(defun td/command-workspace--ensure-tmux-session (session-name project-root)
  "Ensure a tmux session for SESSION-NAME exists in PROJECT-ROOT."
  (unless (td/command-workspace--tmux-session-exists-p session-name)
    (td/command-workspace--tmux-run
     (list "new-session"
           "-d"
           "-s" session-name
           "-c" (file-name-as-directory project-root)))))

(defun td/command-workspace--require-feature (feature)
  "Return non-nil when FEATURE can be loaded.

Initialize `package' once as a fallback for long-lived Emacs daemons
that were started before a newly installed package was activated."
  (or (require feature nil t)
      (progn
        (require 'package)
        (package-initialize)
        (require feature nil t))))

(defun td/command-workspace--backend ()
  "Resolve an available terminal backend."
  (pcase td/command-workspace-terminal-backend
    ('eat (cond ((td/command-workspace--require-feature 'eat) 'eat)
                ((td/command-workspace--require-feature 'vterm) 'vterm)
                (t (user-error "No terminal backend available (install eat or vterm)"))))
    ('vterm (cond ((td/command-workspace--require-feature 'vterm) 'vterm)
                  ((td/command-workspace--require-feature 'eat) 'eat)
                  (t (user-error "No terminal backend available (install eat or vterm)"))))
    (_ (cond ((td/command-workspace--require-feature 'eat) 'eat)
             ((td/command-workspace--require-feature 'vterm) 'vterm)
             (t (user-error "No terminal backend available (install eat or vterm)"))))))

(defun td/command-workspace--launch-eat (session-name project-root)
  "Open an Eat terminal and attach to SESSION-NAME.
Reuse an existing terminal session when available."
  (let ((program (format "tmux attach -t %s" (shell-quote-argument session-name))))
    (let ((eat-buffer-name (td/command-workspace--buffer-name session-name))
          (default-directory (file-name-as-directory project-root)))
      (eat program))))

(defun td/command-workspace--launch-vterm (session-name project-root)
  "Open a vterm terminal and attach to SESSION-NAME."
  (let* ((buffer-name (td/command-workspace--buffer-name session-name))
         (existing (gethash session-name td/command-workspace--session-buffers))
         (was-attached
          (and existing
               (buffer-live-p existing)
               (with-current-buffer existing
                 td/command-workspace--has-attached-p))))
    (let ((buffer (vterm)))
      (rename-buffer buffer-name t)
      (with-current-buffer buffer
        (setq-local default-directory (file-name-as-directory project-root))
        (unless was-attached
          (setq-local td/command-workspace--has-attached-p nil)
          (run-at-time
           0.15 nil
           (lambda (buf name)
             (when (and (buffer-live-p buf)
                        (with-current-buffer buf
                          (not td/command-workspace--has-attached-p)))
               (with-current-buffer buf
                 (setq-local td/command-workspace--has-attached-p t)
                 (vterm-send-string (format "tmux attach -t %s\n" name)))))
           buffer
           session-name)))
      buffer)))



(defun td/command-workspace--terminal-buffer (session-name project-root)
  "Return the terminal buffer for SESSION-NAME in PROJECT-ROOT."
  (td/command-workspace--ensure-tmux-session session-name project-root)
  (or (and-let* ((buffer (gethash session-name td/command-workspace--session-buffers))
                (live-buffer (when (buffer-live-p buffer) buffer)))
        live-buffer)
      (let ((buffer (pcase (td/command-workspace--backend)
                      ('eat (td/command-workspace--launch-eat session-name project-root))
                      (_ (td/command-workspace--launch-vterm session-name project-root)))))
        (puthash session-name buffer td/command-workspace--session-buffers)
        buffer)))

(defun td/command-workspace--open-dashboard (project-root &optional file)
  "Open a workspace dashboard for PROJECT-ROOT.

When FILE is non-nil, visit it. Otherwise prefer `magit-status' in git repos
and `dired' as the fallback."
  (let ((default-directory (file-name-as-directory project-root)))
    (cond
     ((and file (file-exists-p file))
      (find-file file))
     ((and (fboundp 'magit-status)
           (file-exists-p (expand-file-name ".git" project-root)))
      (magit-status))
     (t
      (dired project-root)))))

(defun td/command-workspace-open-project-workspace (&optional project-root file)
  "Switch to project workspace for PROJECT-ROOT and open its dashboard.

When PROJECT-ROOT is nil, use the current project. Outside a project, use
the current file's directory or `default-directory'. When FILE is non-nil,
open it as the workspace dashboard."
  (interactive)
  (let* ((resolved-root (or project-root (td/command-workspace--current-root)))
         (file (or file
                   (td/command-workspace--current-dashboard-file resolved-root)))
         (tab-name (td/command-workspace--tab-name resolved-root))
         (tab-exists-p (td/command-workspace--tab-exists-p tab-name)))
    (tab-bar-mode 1)
    (tab-bar-switch-to-tab tab-name)
    (when (or file (not tab-exists-p))
      (td/command-workspace--open-dashboard resolved-root file))
    resolved-root))

(defun td/command-workspace-open-project-terminal (&optional project-root)
  "Open the default terminal for PROJECT-ROOT's workspace."
  (interactive)
  (let* ((project-root (or project-root (td/command-workspace--current-root)))
         (session-name (td/command-workspace--session-name project-root))
         (buffer (td/command-workspace--terminal-buffer session-name project-root)))
    (td/command-workspace-open-project-workspace project-root)
    (pop-to-buffer buffer)))

(defun td/command-workspace-run-project-agent (prompt &optional project-root)
  "Run td-agent with PROMPT in PROJECT-ROOT's tmux-backed workspace."
  (interactive
   (list (read-string "Agent prompt: ")))
  (let* ((project-root (or project-root (td/command-workspace--current-root)))
         (session-name (td/command-workspace--session-name project-root))
         (command (td/command-workspace--tmux-agent-command
                   (list "run" prompt))))
    (td/command-workspace--tmux-new-window session-name project-root "agent" command)
    (td/command-workspace-open-project-workspace project-root)
    (pop-to-buffer (td/command-workspace--terminal-buffer session-name project-root))))

(defun td/command-workspace-agent-auth-status (&optional project-root)
  "Show td-agent auth status in PROJECT-ROOT's tmux-backed workspace."
  (interactive)
  (let* ((project-root (or project-root (td/command-workspace--current-root)))
         (session-name (td/command-workspace--session-name project-root))
         (command (td/command-workspace--tmux-agent-command
                   (list "auth-status"))))
    (td/command-workspace--tmux-new-window session-name project-root "agent-auth" command)
    (td/command-workspace-open-project-workspace project-root)
    (pop-to-buffer (td/command-workspace--terminal-buffer session-name project-root))))

(defun td/command-workspace--json-get (key object)
  "Return KEY from parsed JSON OBJECT."
  (alist-get key object nil nil #'string=))

(defun td/command-workspace--json-get-any (keys object)
  "Return the first present member among KEYS in parsed JSON OBJECT."
  (seq-some (lambda (key)
              (td/command-workspace--json-get key object))
            keys))

(defun td/command-workspace--agent-event-type (event)
  "Return CLI Agent EVENT's event type."
  (td/command-workspace--json-get "event" event))

(defun td/command-workspace--agent-event-session-id (event)
  "Return CLI Agent EVENT's Transcript Session id."
  (td/command-workspace--json-get "session_id" event))

(defun td/command-workspace--agent-event-project-root (event)
  "Return EVENT's project root when it carries one."
  (td/command-workspace--json-get-any
   '("cwd" "project_root" "project_path")
   event))

(defun td/command-workspace--agent-json-value-string (value)
  "Return a compact string representation of parsed JSON VALUE."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((numberp value) (number-to-string value))
   ((eq value t) "true")
   (t
    (condition-case nil
        (json-serialize value)
      (error (format "%S" value))))))

(defun td/command-workspace--agent-preview-string (value)
  "Return VALUE as a short Agent Action preview string."
  (let ((text (td/command-workspace--agent-json-value-string value)))
    (string-trim
     (truncate-string-to-width text 600 nil nil "..."))))

(defun td/command-workspace--agent-action-permission-event-p (&optional event)
  "Return non-nil when EVENT is a permission action event."
  (string-equal
   (td/command-workspace--agent-event-type
    (or event td/command-workspace--agent-action-event))
   "permission_request"))

(defun td/command-workspace--agent-action-question-event-p (&optional event)
  "Return non-nil when EVENT is a question action event."
  (string-equal
   (td/command-workspace--agent-event-type
    (or event td/command-workspace--agent-action-event))
   "question_asked"))

(defun td/command-workspace--agent-permission-request-id (event)
  "Return EVENT's Agent Permission Flow request id."
  (td/command-workspace--json-get-any
   '("request_id" "permission_request_id" "id")
   event))

(defun td/command-workspace--agent-permission-tool-name (event)
  "Return EVENT's permissioned tool name."
  (or (td/command-workspace--json-get-any '("tool_name" "tool") event)
      "unknown"))

(defun td/command-workspace--agent-permission-decision (event)
  "Return EVENT's Permission Decision."
  (or (td/command-workspace--json-get-any '("decision" "reply") event)
      "unknown"))

(defun td/command-workspace--agent-permission-input-preview (event)
  "Return EVENT's permission request input preview."
  (or (when-let* ((preview
                   (td/command-workspace--json-get-any
                    '("input_preview" "tool_input_preview" "preview")
                    event)))
        (td/command-workspace--agent-preview-string preview))
      (when-let* ((tool-input
                   (td/command-workspace--json-get-any
                    '("tool_input" "input" "arguments")
                    event)))
        (td/command-workspace--agent-preview-string tool-input))
      ""))

(defun td/command-workspace--agent-permission-context-lines (event)
  "Return project/session context lines for permission EVENT."
  (delq
   nil
   (list
    (when-let* ((request-id
                 (td/command-workspace--agent-permission-request-id event)))
      (format "Request: %s" request-id))
    (when-let* ((session-id
                 (td/command-workspace--agent-event-session-id event)))
      (format "Session: %s" session-id))
    (when-let* ((project (td/command-workspace--json-get "project" event)))
      (format "Project: %s" project))
    (when-let* ((cwd (td/command-workspace--agent-event-project-root event)))
      (format "Root: %s" (abbreviate-file-name cwd))))))

(defun td/command-workspace--agent-permission-summary (event)
  "Return the text shown for permission EVENT in the Agent Action Transient."
  (string-join
   (append
    (list (format "Tool: %s"
                  (td/command-workspace--agent-permission-tool-name event)))
    (td/command-workspace--agent-permission-context-lines event)
    (let ((preview
           (td/command-workspace--agent-permission-input-preview event)))
      (unless (string-empty-p preview)
        (list "Input preview:" preview))))
   "\n"))

(defun td/command-workspace--agent-question-id (event)
  "Return EVENT's agent question id."
  (td/command-workspace--json-get-any
   '("question_id" "request_id" "id")
   event))

(defun td/command-workspace--agent-question-text (event)
  "Return EVENT's question text."
  (or (td/command-workspace--json-get-any
       '("question" "prompt" "message" "text")
       event)
      ""))

(defun td/command-workspace--agent-question-context-preview (event)
  "Return EVENT's question context preview."
  (or (when-let* ((context
                   (td/command-workspace--json-get-any
                    '("context" "question_context" "details")
                    event)))
        (td/command-workspace--agent-preview-string context))
      ""))

(defun td/command-workspace--agent-question-context-lines (event)
  "Return project/session context lines for question EVENT."
  (delq
   nil
   (list
    (when-let* ((question-id
                 (td/command-workspace--agent-question-id event)))
      (format "Question ID: %s" question-id))
    (when-let* ((session-id
                 (td/command-workspace--agent-event-session-id event)))
      (format "Session: %s" session-id))
    (when-let* ((project (td/command-workspace--json-get "project" event)))
      (format "Project: %s" project))
    (when-let* ((cwd (td/command-workspace--agent-event-project-root event)))
      (format "Root: %s" (abbreviate-file-name cwd))))))

(defun td/command-workspace--agent-question-summary (event)
  "Return the text shown for question EVENT in the Agent Action Transient."
  (let ((question
         (td/command-workspace--agent-question-text event))
        (context
         (td/command-workspace--agent-question-context-preview event)))
    (string-join
     (append
      (list (format "Question: %s"
                    (if (string-empty-p question)
                        "(no question text)"
                      question)))
      (td/command-workspace--agent-question-context-lines event)
      (unless (string-empty-p context)
        (list "Context:" context)))
     "\n")))

(defun td/command-workspace--agent-action-summary (event)
  "Return the text shown for EVENT in the Agent Action Transient."
  (pcase (td/command-workspace--agent-event-type event)
    ("permission_request"
     (td/command-workspace--agent-permission-summary event))
    ("question_asked"
     (td/command-workspace--agent-question-summary event))
    (_
     (string-join
      (delq
       nil
       (list
        (format "Event: %s"
                (or (td/command-workspace--agent-event-type event) "unknown"))
        (when-let* ((session-id
                     (td/command-workspace--agent-event-session-id event)))
          (format "Session: %s" session-id))))
      "\n"))))

(defun td/command-workspace--agent-action-transient-description ()
  "Return the current Agent Action Transient description."
  (if td/command-workspace--agent-action-event
      (td/command-workspace--agent-action-summary
       td/command-workspace--agent-action-event)
    "No agent action selected."))

(defun td/command-workspace--agent-event-id (event)
  "Return EVENT's stable event id when present."
  (td/command-workspace--json-get-any '("event_id" "eventId") event))

(defun td/command-workspace--agent-event-timestamp (event)
  "Return EVENT's timestamp when present."
  (td/command-workspace--json-get-any '("timestamp" "time") event))

(defun td/command-workspace--agent-event-task-title (event)
  "Return EVENT's Agent Task title when present."
  (td/command-workspace--json-get-any
   '("task_title" "title" "task")
   event))

(defun td/command-workspace--agent-notification-kind (event)
  "Return the Agent Notification kind for EVENT, or nil."
  (pcase (td/command-workspace--agent-event-type event)
    ("permission_request" 'permission)
    ("question_asked" 'question)
    ("stop" 'completed)
    ((or "error" "session_error" "resume_error") 'error)
    (_ nil)))

(defun td/command-workspace--agent-notification-id (event kind)
  "Return a stable notification id for EVENT of KIND."
  (format "%s:%s:%s"
          kind
          (or (td/command-workspace--agent-event-session-id event) "_")
          (or (td/command-workspace--agent-event-id event)
              (pcase kind
                ('permission
                 (td/command-workspace--agent-permission-request-id event))
                ('question
                 (td/command-workspace--agent-question-id event))
                (_ nil))
              (td/command-workspace--json-get "id" event)
              (td/command-workspace--agent-event-timestamp event)
              (secure-hash 'sha1 (prin1-to-string event)))))

(defun td/command-workspace--agent-notification-title (kind event)
  "Return a short Agent Notification title for KIND and EVENT."
  (pcase kind
    ('permission
     (format "Permission request: %s"
             (td/command-workspace--agent-permission-tool-name event)))
    ('question
     (let ((question (td/command-workspace--agent-question-text event)))
       (if (string-empty-p question)
           "Agent question"
         (format "Agent question: %s"
                 (truncate-string-to-width question 72 nil nil "...")))))
    ('completed
     (or (td/command-workspace--agent-event-task-title event)
         "Agent cell completed"))
    ('error "Agent session errored")
    (_ "Agent event")))

(defun td/command-workspace--agent-notification-body (kind event)
  "Return Agent Notification body text for KIND and EVENT."
  (pcase kind
    ('permission
     (td/command-workspace--agent-permission-summary event))
    ('question
     (td/command-workspace--agent-question-summary event))
    ('completed
     (or (td/command-workspace--json-get-any
          '("summary" "message" "response")
          event)
         (when-let* ((session-id
                      (td/command-workspace--agent-event-session-id event)))
           (format "Session: %s" session-id))
         "A Notebook Cell completed."))
    ('error
     (or (td/command-workspace--json-get-any
          '("error" "message" "summary" "details")
          event)
         (when-let* ((status
                      (td/command-workspace--json-get "exit_status" event)))
           (format "td-agent exited with status %s" status))
         "A Transcript Session reported an error."))
    (_
     (td/command-workspace--agent-action-summary event))))

(defun td/command-workspace--agent-notification-message (notification)
  "Show NOTIFICATION through an Emacs message fallback."
  (message "td-agent: %s" (plist-get notification :title))
  'message)

(defun td/command-workspace--agent-notification-present (notification)
  "Present NOTIFICATION inside Emacs and return the presentation kind."
  (let ((shown
         (when (functionp td/command-workspace-agent-notification-popup-function)
           (condition-case nil
               (funcall td/command-workspace-agent-notification-popup-function
                        notification)
             (error nil)))))
    (if shown
        'popup
      (td/command-workspace--agent-notification-message notification))))

(defun td/command-workspace--agent-notification-record (event)
  "Record and present the Agent Notification derived from EVENT."
  (when-let* ((kind (td/command-workspace--agent-notification-kind event)))
    (let* ((id (td/command-workspace--agent-notification-id event kind))
           (existing (gethash id td/command-workspace--agent-notifications))
           (notification
            (or existing (list :id id :state 'unread))))
      (setq notification (plist-put notification :kind kind))
      (setq notification (plist-put notification :event event))
      (setq notification
            (plist-put notification
                       :session-id
                       (td/command-workspace--agent-event-session-id event)))
      (setq notification
            (plist-put notification
                       :project-root
                       (td/command-workspace--agent-event-project-root event)))
      (setq notification
            (plist-put notification
                       :created-at
                       (or (td/command-workspace--agent-event-timestamp event)
                           (format-time-string "%Y-%m-%dT%H:%M:%S%z"))))
      (setq notification
            (plist-put notification
                       :title
                       (td/command-workspace--agent-notification-title
                        kind event)))
      (setq notification
            (plist-put notification
                       :body
                       (td/command-workspace--agent-notification-body
                        kind event)))
      (unless existing
        (setq td/command-workspace--agent-notification-order
              (append td/command-workspace--agent-notification-order
                      (list id))))
      (puthash id notification td/command-workspace--agent-notifications)
      (td/command-workspace--agent-action-queue-refresh-visible)
      (unless existing
        (setq notification
              (plist-put notification
                         :presented-as
                         (td/command-workspace--agent-notification-present
                          notification)))
        (puthash id notification td/command-workspace--agent-notifications))
      notification)))

(defun td/command-workspace--agent-notifications-mark-session-read (session-id)
  "Mark Agent Notifications for SESSION-ID as read."
  (let ((changed nil))
    (when (and session-id (not (string-empty-p session-id)))
      (maphash
       (lambda (id notification)
         (when (string-equal (plist-get notification :session-id) session-id)
           (setq notification (plist-put notification :state 'read))
           (puthash id notification
                    td/command-workspace--agent-notifications)
           (setq changed t)))
       td/command-workspace--agent-notifications))
    (when changed
      (td/command-workspace--agent-action-queue-refresh-visible))
    changed))

(defun td/command-workspace--agent-permission-pending-p (event)
  "Return non-nil when EVENT's permission request is still pending."
  (let* ((request-id
          (td/command-workspace--agent-permission-request-id event))
         (record
          (and request-id
               (gethash request-id
                        td/command-workspace--agent-permission-requests))))
    (or (null record)
        (eq (plist-get record :state) 'blocked))))

(defun td/command-workspace--agent-question-pending-p (event)
  "Return non-nil when EVENT's question is still pending."
  (let* ((question-key
          (or (td/command-workspace--agent-question-id event)
              (td/command-workspace--agent-event-session-id event)))
         (record
          (and question-key
               (gethash question-key td/command-workspace--agent-questions))))
    (or (null record)
        (eq (plist-get record :state) 'blocked))))

(defun td/command-workspace--agent-question-mark-answered (event)
  "Mark EVENT's question as answered in the Agent Action Queue state."
  (when-let* ((question-key
               (or (td/command-workspace--agent-question-id event)
                   (td/command-workspace--agent-event-session-id event))))
    (let ((record
           (or (gethash question-key td/command-workspace--agent-questions)
               (list :event event))))
      (setq record (plist-put record :state 'answered))
      (puthash question-key record td/command-workspace--agent-questions)
      (td/command-workspace--agent-action-queue-refresh-visible))))

(defun td/command-workspace--agent-action-queue-item-kind-label (kind)
  "Return a display label for queue item KIND."
  (pcase kind
    ('permission "Permission")
    ('question "Question")
    ('completed "Completed")
    ('error "Error")
    (_ "Event")))

(defun td/command-workspace--agent-action-queue-summary (notification)
  "Return one-line queue summary text for NOTIFICATION."
  (truncate-string-to-width
   (replace-regexp-in-string
    "[ \t\n]+" " "
    (string-trim
     (or (plist-get notification :body)
         (plist-get notification :title)
         "")))
   160 nil nil "..."))

(defun td/command-workspace--agent-action-queue-item (notification
                                                       &optional include-read)
  "Return a queue item plist for NOTIFICATION.

When INCLUDE-READ is non-nil, return an item even if NOTIFICATION no longer
belongs in the visible queue."
  (let* ((kind (plist-get notification :kind))
         (event (plist-get notification :event))
         (pending
          (pcase kind
            ('permission
             (td/command-workspace--agent-permission-pending-p event))
            ('question
             (td/command-workspace--agent-question-pending-p event))
            (_ nil)))
         (unread (eq (plist-get notification :state) 'unread))
         (visible
          (or include-read
              (and (memq kind '(permission question)) pending)
              (and (memq kind '(completed error)) unread))))
    (when visible
      (list
       :id (plist-get notification :id)
       :kind kind
       :kind-label (td/command-workspace--agent-action-queue-item-kind-label
                    kind)
       :state (cond
               (pending 'pending)
               (unread 'unread)
               (t 'read))
       :event event
       :notification notification
       :actionable (and pending (memq kind '(permission question)))
       :session-id (plist-get notification :session-id)
       :project-root (plist-get notification :project-root)
       :title (plist-get notification :title)
       :summary (td/command-workspace--agent-action-queue-summary
                 notification)))))

(defun td/command-workspace--agent-action-queue-filter-match-p (item filter)
  "Return non-nil when ITEM belongs under FILTER."
  (let ((filter (cond
                 ((null filter) 'all)
                 ((stringp filter) (intern filter))
                 (t filter))))
    (pcase filter
      ('all t)
      ('actionable (plist-get item :actionable))
      ('unread
       (eq (plist-get (plist-get item :notification) :state) 'unread))
      ((or 'permission 'question 'completed 'error)
       (eq (plist-get item :kind) filter))
      (_ t))))

(defun td/command-workspace--agent-action-queue-items (&optional filter)
  "Return current Agent Action Queue items, optionally narrowed by FILTER."
  (let (items)
    (dolist (id (reverse td/command-workspace--agent-notification-order))
      (when-let* ((notification
                   (gethash id td/command-workspace--agent-notifications))
                  (item
                   (td/command-workspace--agent-action-queue-item
                    notification)))
        (when (td/command-workspace--agent-action-queue-filter-match-p
               item filter)
          (push item items))))
    (nreverse items)))

(defun td/command-workspace--agent-action-queue-refresh-visible ()
  "Refresh the visible Agent Action Queue buffer when it exists."
  (when-let* ((buffer (get-buffer "*td-agent action queue*")))
    (with-current-buffer buffer
      (when (derived-mode-p 'td/command-workspace-agent-action-queue-mode)
        (td/command-workspace--agent-action-queue-refresh)))))

(defun td/command-workspace--agent-action-queue-activate-item (item)
  "Activate Agent Action Queue ITEM."
  (let ((event (plist-get item :event)))
    (td/command-workspace--agent-action-open-notebook-buffer event)
    (when (plist-get item :actionable)
      (td/command-workspace-agent-action-transient event))
    item))

(defun td/command-workspace--agent-notification-display-popup (notification)
  "Display NOTIFICATION as a minimal Emacs-local popup."
  (when (not noninteractive)
    (let ((buffer
           (get-buffer-create
            td/command-workspace-agent-notification-popup-buffer-name)))
      (with-current-buffer buffer
        (td/command-workspace-agent-notification-mode)
        (setq-local td/command-workspace--agent-notification-current-id
                    (plist-get notification :id))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (plist-get notification :title) "\n\n")
          (insert (or (plist-get notification :body) ""))
          (insert "\n\nRET opens this Agent Notification.\n")
          (goto-char (point-min))))
      (when (display-buffer
             buffer
             '((display-buffer-reuse-window display-buffer-in-side-window)
               (side . bottom)
               (slot . 1)
               (window-height . fit-window-to-buffer)))
        t))))

(defun td/command-workspace--agent-notebook-note-event-session (event buffer)
  "Remember that EVENT's session is rendered by BUFFER."
  (when-let* ((session-id
               (td/command-workspace--agent-event-session-id event)))
    (puthash session-id buffer td/command-workspace--agent-notebook-session-buffers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq-local td/command-workspace--agent-notebook-session-id session-id)))))

(defun td/command-workspace--agent-permission-record (event buffer)
  "Record permission request EVENT for BUFFER."
  (when-let* ((request-id
               (td/command-workspace--agent-permission-request-id event)))
    (puthash request-id
             (list :event event :buffer buffer :state 'blocked)
             td/command-workspace--agent-permission-requests)
    (td/command-workspace--agent-action-queue-refresh-visible)))

(defun td/command-workspace--agent-permission-mark-replied (event)
  "Mark EVENT's matching permission request as replied."
  (when-let* ((request-id
               (td/command-workspace--agent-permission-request-id event)))
    (let ((record (or (gethash request-id
                               td/command-workspace--agent-permission-requests)
                      (list :event event))))
      (setq record (plist-put record :reply-event event))
      (setq record (plist-put record :state 'replied))
      (puthash request-id record
               td/command-workspace--agent-permission-requests)
      (td/command-workspace--agent-action-queue-refresh-visible))))

(defun td/command-workspace--agent-question-record (event buffer)
  "Record question EVENT for BUFFER."
  (when-let* ((question-key
               (or (td/command-workspace--agent-question-id event)
                   (td/command-workspace--agent-event-session-id event))))
    (puthash question-key
             (list :event event :buffer buffer :state 'blocked)
             td/command-workspace--agent-questions)
    (td/command-workspace--agent-action-queue-refresh-visible)))

(defun td/command-workspace--render-agent-event (event)
  "Insert a readable representation of one transcript EVENT."
  (let ((type (td/command-workspace--json-get "event" event))
        (timestamp (td/command-workspace--json-get "timestamp" event))
        (session-id (td/command-workspace--json-get "session_id" event))
        (project (td/command-workspace--json-get "project" event)))
    (insert (format "* %s  %s\n" (or timestamp "") (or type "event")))
    (when session-id
      (insert (format ":session: %s\n" session-id)))
    (when project
      (insert (format ":project: %s\n" project)))
    (pcase type
      ("prompt_submit"
       (when-let* ((query (td/command-workspace--json-get "query" event)))
         (insert "\n#+begin_quote\n" query "\n#+end_quote\n")))
      ("tool_start"
       (insert (format "\nTool: %s\n"
                       (or (td/command-workspace--json-get "tool_name" event)
                           ""))))
      ("tool_complete"
       (insert (format "\nTool: %s\n\n#+begin_example\n%s\n#+end_example\n"
                       (or (td/command-workspace--json-get "tool_name" event)
                           "")
                       (or (td/command-workspace--json-get "response" event)
                           ""))))
      ("permission_request"
       (insert
        (format "\n%s\n"
                (td/command-workspace--agent-permission-summary event))))
      ("permission_replied"
       (insert
        (format "\nRequest: %s\nDecision: %s\n"
                (or (td/command-workspace--agent-permission-request-id event) "")
                (td/command-workspace--agent-permission-decision event))))
      ("question_asked"
       (insert
        (format "\n%s\n"
                (td/command-workspace--agent-question-summary event))))
      ("stop"
       (when-let* ((summary (td/command-workspace--json-get "summary" event)))
         (insert "\n" summary "\n"))))
    (insert "\n")))

(defun td/command-workspace-open-agent-transcript ()
  "Open the read-only td-agent transcript event log."
  (interactive)
  (unless (file-exists-p td/command-workspace-agent-transcript-log)
    (user-error "No transcript log at %s"
                (abbreviate-file-name td/command-workspace-agent-transcript-log)))
  (let ((buffer (get-buffer-create "*td-agent transcript*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "#+title: td-agent transcript\n\n")
        (with-temp-buffer
          (insert-file-contents td/command-workspace-agent-transcript-log)
          (dolist (line (split-string (buffer-string) "\n" t))
            (condition-case nil
                (td/command-workspace--render-agent-event
                 (json-parse-string line
                                    :object-type 'alist
                                    :array-type 'list
                                    :false-object nil
                                    :null-object nil))
              (error
               (with-current-buffer buffer
                 (insert "* malformed event\n\n#+begin_example\n"
                         line
                         "\n#+end_example\n\n"))))))
        (goto-char (point-min))
        (org-mode)
        (read-only-mode 1)))
    (pop-to-buffer buffer)))

(defvar td/command-workspace-agent-notebook-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'td/command-workspace-agent-notebook-submit)
    (define-key map (kbd "C-c C-r") #'td/command-workspace-agent-notebook-refresh-transcript)
    (define-key map (kbd "C-c C-q") #'quit-window)
    map)
  "Keymap for td-agent notebook buffers.")

(define-derived-mode td/command-workspace-agent-notebook-mode text-mode "td-agent"
  "Agent Notebook projection for td-agent runs."
  (setq-local buffer-read-only nil)
  (setq-local truncate-lines nil)
  (setq-local code-cells-eval-region-commands
              (cons
               '(td/command-workspace-agent-notebook-mode
                 . td/command-workspace--agent-notebook-submit-region)
               (and (boundp 'code-cells-eval-region-commands)
                    code-cells-eval-region-commands)))
  (when (featurep 'code-cells)
    (setq-local code-cells-boundary-regexp "^# \\(%+\\).*")
    (code-cells-mode 1))
  (when (td/command-workspace--agent-notebook-restore-draft-markers)
    (td/command-workspace--agent-notebook-protect-buffer)))

(defun td/command-workspace--agent-notebook-submit-region (_beg _end)
  "Submit the current Agent Notebook Draft Cell.

This adapter lets `code-cells-eval' drive `td-agent' notebook cells."
  (td/command-workspace-agent-notebook-submit))

(defun td/command-workspace--agent-notebook-insert-header (project-root
                                                           task-title)
  "Insert an Agent Notebook header for PROJECT-ROOT and TASK-TITLE."
  (insert "# td-agent notebook")
  (when (and task-title (not (string-empty-p task-title)))
    (insert ": " task-title))
  (insert "\n")
  (insert (format "# project: %s\n" (abbreviate-file-name project-root)))
  (when (and task-title (not (string-empty-p task-title)))
    (insert (format "# task_title: %s\n" task-title)))
  (insert "\n"))

(defun td/command-workspace--agent-notebook-buffer-name (project-root
                                                          &optional task-title)
  "Return notebook buffer name for PROJECT-ROOT, optionally titled TASK-TITLE."
  (format "*td-agent notebook: %s*"
          (or (and task-title
                   (not (string-empty-p task-title))
                   task-title)
              (file-name-nondirectory (directory-file-name project-root)))))

(defun td/command-workspace--agent-notebook-clear-draft ()
  "Forget the current Draft Notebook Cell markers."
  (setq-local td/command-workspace--agent-notebook-draft-begin-marker nil)
  (setq-local td/command-workspace--agent-notebook-draft-end-marker nil))

(defun td/command-workspace--agent-notebook-draft-live-p ()
  "Return non-nil when the current buffer has a Draft Notebook Cell."
  (and (markerp td/command-workspace--agent-notebook-draft-begin-marker)
       (markerp td/command-workspace--agent-notebook-draft-end-marker)
       (marker-buffer td/command-workspace--agent-notebook-draft-begin-marker)
       (marker-buffer td/command-workspace--agent-notebook-draft-end-marker)))

(defun td/command-workspace--agent-notebook-draft-bounds-from-buffer ()
  "Return body bounds for the visible Draft Notebook Cell, when present."
  (save-excursion
    (let (cell-start cell-end)
      (goto-char (point-min))
      (while (re-search-forward "^# %%+ Draft Notebook Cell.*$" nil t)
        (setq cell-start (line-beginning-position))
        (setq cell-end (line-end-position)))
      (when cell-start
        (goto-char cell-end)
        (forward-line 1)
        (while (looking-at-p "^# :")
          (forward-line 1))
        (when (looking-at-p "^$")
          (forward-line 1))
        (let ((body-start (point))
              (body-end
               (save-excursion
                 (if (re-search-forward "^# %%+" nil t)
                     (match-beginning 0)
                   (point-max)))))
          (cons body-start body-end))))))

(defun td/command-workspace--agent-notebook-restore-draft-markers ()
  "Restore Draft Notebook Cell markers from visible code-cells text."
  (unless (td/command-workspace--agent-notebook-draft-live-p)
    (when-let* ((bounds
                 (td/command-workspace--agent-notebook-draft-bounds-from-buffer)))
      (setq-local td/command-workspace--agent-notebook-draft-begin-marker
                  (copy-marker (car bounds) nil))
      (setq-local td/command-workspace--agent-notebook-draft-end-marker
                  (copy-marker (cdr bounds) t))))
  (td/command-workspace--agent-notebook-draft-live-p))

(defun td/command-workspace--agent-notebook-protect-buffer ()
  "Make prior notebook projection text read-only and leave the Draft editable."
  (let ((inhibit-read-only t))
    (add-text-properties (point-min) (point-max) '(read-only t))
    (when (td/command-workspace--agent-notebook-draft-live-p)
      (let ((start (marker-position
                    td/command-workspace--agent-notebook-draft-begin-marker)))
        (remove-text-properties (max (point-min) (1- start))
                                (point-max)
                                '(read-only t))))))

(defun td/command-workspace--agent-notebook-draft-prompt ()
  "Return the current Draft Notebook Cell prompt."
  (unless (td/command-workspace--agent-notebook-restore-draft-markers)
    (user-error "No Draft Notebook Cell in this notebook"))
  (string-trim
   (buffer-substring-no-properties
    td/command-workspace--agent-notebook-draft-begin-marker
    td/command-workspace--agent-notebook-draft-end-marker)))

(defun td/command-workspace--agent-notebook-insert-draft-cell (&optional prompt)
  "Insert one Draft Notebook Cell containing PROMPT."
  (let ((inhibit-read-only t)
        (prompt (or prompt "")))
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (insert "\n# %% Draft Notebook Cell\n"
            "# :state: draft\n\n")
    (setq-local td/command-workspace--agent-notebook-draft-begin-marker
                (copy-marker (point) nil))
    (unless (string-empty-p prompt)
      (insert prompt))
    (setq-local td/command-workspace--agent-notebook-draft-end-marker
                (copy-marker (point) nil))
    (insert "\n")
    (set-marker-insertion-type
     td/command-workspace--agent-notebook-draft-end-marker t))
  (td/command-workspace--agent-notebook-protect-buffer)
  (when (td/command-workspace--agent-notebook-draft-live-p)
    (goto-char td/command-workspace--agent-notebook-draft-begin-marker)))

(defun td/command-workspace--agent-notebook-initialize (buffer project-root
                                                               &optional
                                                               task-title
                                                               prompt)
  "Initialize BUFFER as a fresh Agent Notebook for PROJECT-ROOT."
  (with-current-buffer buffer
    (td/command-workspace-agent-notebook-mode)
    (setq-local td/command-workspace--agent-notebook-project-root
                (file-name-as-directory project-root))
    (setq-local td/command-workspace--agent-notebook-session-id nil)
    (setq-local td/command-workspace--agent-notebook-task-title task-title)
    (setq-local td/command-workspace--agent-notebook-output-block-open-p nil)
    (setq-local td/command-workspace--agent-notebook-process nil)
    (setq-local td/command-workspace--agent-notebook-osc-fragment "")
    (setq-local td/command-workspace--agent-notebook-osc-active-p nil)
    (setq-local td/command-workspace--agent-notebook-blocked-permission-request-id nil)
    (setq-local td/command-workspace--agent-notebook-blocked-question-id nil)
    (td/command-workspace--agent-notebook-clear-draft)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (td/command-workspace--agent-notebook-insert-header project-root
                                                          task-title))
    (td/command-workspace--agent-notebook-insert-draft-cell prompt))
  buffer)

(defun td/command-workspace--agent-notebook-buffer (project-root)
  "Return the notebook buffer for PROJECT-ROOT."
  (let ((buffer (get-buffer-create
                 (td/command-workspace--agent-notebook-buffer-name project-root))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'td/command-workspace-agent-notebook-mode)
        (td/command-workspace-agent-notebook-mode)
        (setq-local td/command-workspace--agent-notebook-project-root
                    (file-name-as-directory project-root))
        (setq-local td/command-workspace--agent-notebook-session-id nil)
        (setq-local td/command-workspace--agent-notebook-task-title nil)
        (setq-local td/command-workspace--agent-notebook-output-block-open-p nil)
        (setq-local td/command-workspace--agent-notebook-process nil)
        (td/command-workspace--agent-notebook-clear-draft)
        (setq-local td/command-workspace--agent-notebook-blocked-permission-request-id nil)
        (setq-local td/command-workspace--agent-notebook-blocked-question-id nil)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (td/command-workspace--agent-notebook-insert-header project-root nil)
          (insert "# %% Notes\n\n")
          (insert "C-c C-c submits the editable draft cell. C-c C-r refreshes transcript events.\n\n"))))
    buffer))

(defun td/command-workspace-new-agent-notebook (title &optional project-root
                                                     initial-prompt)
  "Create a new Agent Notebook for a titled Agent Task.

TITLE is passed to `td-agent run --title' when the first Draft Notebook Cell is
submitted.  INITIAL-PROMPT seeds the editable Draft Notebook Cell."
  (interactive
   (list (read-string "Agent task title: ")
         nil
         nil))
  (let* ((project-root (or project-root (td/command-workspace--current-root)))
         (title (string-trim title))
         (buffer (generate-new-buffer
                  (td/command-workspace--agent-notebook-buffer-name
                   project-root title))))
    (when (string-empty-p title)
      (user-error "Agent task title is required"))
    (td/command-workspace--agent-notebook-initialize
     buffer project-root title initial-prompt)
    (td/command-workspace-open-project-workspace project-root)
    (pop-to-buffer buffer)))

(defun td/command-workspace-open-agent-notebook (&optional project-root)
  "Open the Emacs-native td-agent notebook for PROJECT-ROOT."
  (interactive)
  (let* ((project-root (or project-root (td/command-workspace--current-root)))
         (buffer (td/command-workspace--agent-notebook-buffer project-root)))
    (td/command-workspace-open-project-workspace project-root)
    (prog1 (pop-to-buffer buffer)
      (when-let* ((session-id
                   (with-current-buffer buffer
                     td/command-workspace--agent-notebook-session-id)))
        (td/command-workspace--agent-notifications-mark-session-read
         session-id)))))

(defun td/command-workspace--agent-notebook-insert (&rest strings)
  "Insert STRINGS in the current read-only notebook buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (dolist (string strings)
      (when string
        (insert string)))
    (goto-char (point-max)))
  (td/command-workspace--agent-notebook-protect-buffer))

(defun td/command-workspace--agent-notebook-close-output-block ()
  "Close the current notebook output example block when one is open."
  (when td/command-workspace--agent-notebook-output-block-open-p
    (td/command-workspace--agent-notebook-insert "\n```\n")
    (setq-local td/command-workspace--agent-notebook-output-block-open-p nil)))

(defun td/command-workspace--agent-notebook-open-output-block (&optional title)
  "Open a notebook output example block named TITLE when none is open."
  (unless td/command-workspace--agent-notebook-output-block-open-p
    (td/command-workspace--agent-notebook-insert
     (format "\n%s:\n\n```text\n" (or title "Output")))
    (setq-local td/command-workspace--agent-notebook-output-block-open-p t)))

(defun td/command-workspace--agent-notebook-render-permission-request (event)
  "Render permission request EVENT as a Blocked Notebook Cell marker."
  (td/command-workspace--agent-notebook-close-output-block)
  (let ((request-id
         (td/command-workspace--agent-permission-request-id event))
        (session-id
         (td/command-workspace--agent-event-session-id event))
        (tool-name
         (td/command-workspace--agent-permission-tool-name event))
        (preview
         (td/command-workspace--agent-permission-input-preview event)))
    (setq-local td/command-workspace--agent-notebook-blocked-permission-request-id
                request-id)
    (td/command-workspace--agent-notebook-insert
     "\n# %% Blocked Permission Request\n"
     "# :state: blocked\n"
     (when request-id (format "# :request_id: %s\n" request-id))
     (when session-id (format "# :session: %s\n" session-id))
     (format "# :tool: %s\n\n" tool-name)
     (td/command-workspace--agent-permission-summary event)
     (unless (string-empty-p preview)
       (format "\n\n```text\n%s\n```\n" preview))
     "\n")))

(defun td/command-workspace--agent-notebook-render-permission-replied (event)
  "Render permission reply EVENT and reopen the running output stream."
  (td/command-workspace--agent-notebook-close-output-block)
  (let ((request-id
         (td/command-workspace--agent-permission-request-id event))
        (decision
         (td/command-workspace--agent-permission-decision event)))
    (when (or (null td/command-workspace--agent-notebook-blocked-permission-request-id)
              (string-equal
               td/command-workspace--agent-notebook-blocked-permission-request-id
               request-id))
      (setq-local td/command-workspace--agent-notebook-blocked-permission-request-id
                  nil))
    (td/command-workspace--agent-notebook-insert
     "\n# %% Permission Replied\n"
     "# :state: running\n"
     (when request-id (format "# :request_id: %s\n" request-id))
     (format "# :decision: %s\n\n" decision)
     (format "Decision: %s\n" decision)))
  (td/command-workspace--agent-notebook-open-output-block "Output Continued"))

(defun td/command-workspace--agent-notebook-render-question-asked (event)
  "Render question EVENT as a Blocked Notebook Cell marker."
  (td/command-workspace--agent-notebook-close-output-block)
  (let ((question-id
         (td/command-workspace--agent-question-id event))
        (session-id
         (td/command-workspace--agent-event-session-id event))
        (question
         (td/command-workspace--agent-question-text event))
        (context
         (td/command-workspace--agent-question-context-preview event)))
    (setq-local td/command-workspace--agent-notebook-blocked-question-id
                question-id)
    (td/command-workspace--agent-notebook-insert
     "\n# %% Blocked Question\n"
     "# :state: blocked\n"
     (when question-id (format "# :question_id: %s\n" question-id))
     (when session-id (format "# :session: %s\n" session-id))
     "\n"
     (td/command-workspace--agent-question-summary event)
     (unless (string-empty-p question)
       (format "\n\n> %s\n" question))
     (unless (string-empty-p context)
       (format "\n```text\n%s\n```\n" context))
     "\n")))

(defun td/command-workspace--agent-notebook-render-stop (event)
  "Render stop EVENT as a Complete Notebook Cell marker."
  (td/command-workspace--agent-notebook-close-output-block)
  (let ((session-id
         (td/command-workspace--agent-event-session-id event))
        (summary
         (or (td/command-workspace--json-get-any
              '("summary" "message" "response")
              event)
             "")))
    (td/command-workspace--agent-notebook-insert
     "\n# %% Complete Notebook Cell\n"
     "# :state: complete\n"
     (when session-id (format "# :session: %s\n" session-id))
     (unless (string-empty-p summary)
       (format "\n%s\n" summary))
     "\n")))

(defun td/command-workspace--agent-notebook-render-session-error (event)
  "Render EVENT as an errored Transcript Session marker."
  (td/command-workspace--agent-notebook-close-output-block)
  (let ((session-id
         (td/command-workspace--agent-event-session-id event))
        (message
         (or (td/command-workspace--agent-notification-body 'error event)
             "")))
    (td/command-workspace--agent-notebook-insert
     "\n# %% Errored Session\n"
     "# :state: errored\n"
     (when session-id (format "# :session: %s\n" session-id))
     (unless (string-empty-p message)
       (format "\n%s\n" message))
     "\n")))

(defun td/command-workspace--agent-notebook-ingest-event (event &optional process)
  "Ingest one CLI Agent EVENT for the current notebook.

When PROCESS is non-nil, store EVENT on PROCESS for later transcript
reconciliation."
  (when (td/command-workspace--agent-notebook-json-event-p event)
    (let ((buffer (current-buffer)))
      (td/command-workspace--agent-notebook-note-event-session event buffer)
      (when process
        (process-put process 'td-agent-events
                     (append (process-get process 'td-agent-events)
                             (list event))))
      (pcase (td/command-workspace--agent-event-type event)
        ("permission_request"
         (td/command-workspace--agent-permission-record event buffer)
         (td/command-workspace--agent-notebook-render-permission-request event)
         (td/command-workspace--agent-notification-record event)
         (setq td/command-workspace--agent-action-event event)
         (when (and td/command-workspace-agent-auto-open-action-transient
                    (not noninteractive))
           (td/command-workspace--agent-action-transient-menu))
         'permission-request)
        ("permission_replied"
         (td/command-workspace--agent-permission-mark-replied event)
         (td/command-workspace--agent-notebook-render-permission-replied event)
         'permission-replied)
        ("question_asked"
         (td/command-workspace--agent-question-record event buffer)
         (td/command-workspace--agent-notebook-render-question-asked event)
         (td/command-workspace--agent-notification-record event)
         (setq td/command-workspace--agent-action-event event)
         (when (and td/command-workspace-agent-auto-open-action-transient
                    (not noninteractive))
           (td/command-workspace--agent-action-transient-menu))
         'question-asked)
        ("stop"
         (unless (td/command-workspace--agent-notebook-reconcile-current-session)
           (td/command-workspace--agent-notebook-render-stop event))
         (td/command-workspace--agent-notification-record event)
         'stop)
        ((or "error" "session_error" "resume_error")
         (td/command-workspace--agent-notebook-render-session-error event)
         (td/command-workspace--agent-notification-record event)
         'error)
        (_ 'event)))))

(defun td/command-workspace--agent-notebook-json-event-p (event)
  "Return non-nil when EVENT is a td-agent JSON event."
  (and (listp event)
       (td/command-workspace--json-get "event" event)
       (td/command-workspace--json-get "session_id" event)))

(defun td/command-workspace--agent-notebook-handle-osc (process body)
  "Store OSC event BODY from PROCESS."
  (when-let* ((event (ignore-errors
                       (json-parse-string body
                                          :object-type 'alist
                                          :array-type 'list
                                          :false-object nil
                                          :null-object nil))))
    (when (td/command-workspace--agent-notebook-json-event-p event)
      (td/command-workspace--agent-notebook-ingest-event event process))))

(defconst td/command-workspace--agent-osc-prefix
  "\e]777;notify;warp://cli-agent;"
  "OSC prefix used by td-agent CLI Agent Events.")

(defun td/command-workspace--agent-osc-partial-prefix-length (text)
  "Return length of TEXT's suffix that starts an OSC prefix."
  (let ((prefix td/command-workspace--agent-osc-prefix)
        (max (min (length text)
                  (1- (length td/command-workspace--agent-osc-prefix))))
        (length 0)
        (candidate 1))
    (while (<= candidate max)
      (when (string-suffix-p (substring prefix 0 candidate) text)
        (setq length candidate))
      (setq candidate (1+ candidate)))
    length))

(defun td/command-workspace--agent-notebook-insert-process-output (process chunk)
  "Insert PROCESS output CHUNK, stripping td-agent OSC events."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((data chunk)
            (position 0)
            (osc-prefix td/command-workspace--agent-osc-prefix))
        (unless td/command-workspace--agent-notebook-osc-active-p
          (unless (string-empty-p
                   td/command-workspace--agent-notebook-osc-fragment)
            (setq data
                  (concat td/command-workspace--agent-notebook-osc-fragment
                          data))
            (setq td/command-workspace--agent-notebook-osc-fragment "")))
        (while (< position (length data))
          (if td/command-workspace--agent-notebook-osc-active-p
              (if-let* ((osc-end (string-match "\a" data position)))
                  (let ((body
                         (concat
                          td/command-workspace--agent-notebook-osc-fragment
                          (substring data position osc-end))))
                    (setq td/command-workspace--agent-notebook-osc-fragment "")
                    (setq td/command-workspace--agent-notebook-osc-active-p nil)
                    (td/command-workspace--agent-notebook-handle-osc
                     process body)
                    (setq position (1+ osc-end)))
                (setq td/command-workspace--agent-notebook-osc-fragment
                      (concat
                       td/command-workspace--agent-notebook-osc-fragment
                       (substring data position)))
                (setq position (length data)))
            (if-let* ((osc-start
                       (string-match (regexp-quote osc-prefix) data position)))
                (progn
                  (td/command-workspace--agent-notebook-insert
                   (substring data position osc-start))
                  (setq td/command-workspace--agent-notebook-osc-active-p t)
                  (setq td/command-workspace--agent-notebook-osc-fragment "")
                  (setq position (+ osc-start (length osc-prefix))))
              (let* ((tail (substring data position))
                     (partial
                      (td/command-workspace--agent-osc-partial-prefix-length
                       tail)))
                (if (zerop partial)
                    (td/command-workspace--agent-notebook-insert tail)
                  (td/command-workspace--agent-notebook-insert
                   (substring tail 0 (- (length tail) partial)))
                  (setq td/command-workspace--agent-notebook-osc-fragment
                        (substring tail (- (length tail) partial))))
                (setq position (length data))))))))))

(defun td/command-workspace--agent-notebook-sentinel (process event)
  "Finalize notebook PROCESS after EVENT."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((status (process-exit-status process)))
        (td/command-workspace--agent-notebook-close-output-block)
        (when (and (integerp status) (not (zerop status)))
          (td/command-workspace--agent-notebook-insert
           (format "\n[td-agent exited %s: %s]\n\n"
                   status
                   (string-trim event)))
          (td/command-workspace--agent-notification-record
           `(("event" . "session_error")
             ,@(when td/command-workspace--agent-notebook-session-id
                 `(("session_id" .
                    ,td/command-workspace--agent-notebook-session-id)))
             ,@(when td/command-workspace--agent-notebook-project-root
                 `(("cwd" .
                    ,td/command-workspace--agent-notebook-project-root)))
             ("exit_status" . ,status)
             ("message" . ,(string-trim event)))))
        (setq td/command-workspace--agent-notebook-process nil)))))

(defun td/command-workspace--agent-run-argv (task-title prompt)
  "Return argv for starting a titled Agent Task with PROMPT."
  (append
   (list (td/command-workspace--agent-executable)
         td/command-workspace-agent-run-command)
   (when (and task-title (not (string-empty-p task-title)))
     (list "--title" task-title))
   (list prompt)))

(defun td/command-workspace--agent-notebook-mark-draft-running ()
  "Turn the current Draft Notebook Cell into a Running Notebook Cell."
  (when (td/command-workspace--agent-notebook-draft-live-p)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (when (search-forward "Draft Notebook Cell" nil t)
          (replace-match "Running Notebook Cell" nil t))
        (goto-char (point-min))
        (when (search-forward ":state: draft" nil t)
          (replace-match ":state: running" nil t))))
    (td/command-workspace--agent-notebook-clear-draft)))

(defun td/command-workspace--agent-notebook-start-running-cell (prompt
                                                               &optional
                                                               session-id)
  "Render PROMPT as a Running Notebook Cell, optionally tied to SESSION-ID."
  (if (td/command-workspace--agent-notebook-draft-live-p)
      (td/command-workspace--agent-notebook-mark-draft-running)
    (td/command-workspace--agent-notebook-insert
     (format "\n# %%%% Running Notebook Cell  %s\n"
             (format-time-string "%Y-%m-%d %H:%M:%S"))
     "# :state: running\n"
     (when session-id (format "# :session: %s\n" session-id))
     "\n"
     prompt
     "\n"))
  (td/command-workspace--agent-notebook-open-output-block))

(defun td/command-workspace--agent-run (prompt project-root task-title buffer)
  "Start a new titled Agent Task for PROMPT in PROJECT-ROOT and stream to BUFFER."
  (let* ((argv (td/command-workspace--agent-run-argv task-title prompt))
         (default-directory (file-name-as-directory project-root))
         (process
          (make-process
           :name "td-agent-notebook"
           :buffer buffer
           :command argv
           :connection-type 'pipe
           :noquery t
           :filter #'td/command-workspace--agent-notebook-insert-process-output
           :sentinel #'td/command-workspace--agent-notebook-sentinel)))
    (with-current-buffer buffer
      (setq td/command-workspace--agent-notebook-project-root project-root)
      (setq td/command-workspace--agent-notebook-task-title task-title)
      (setq td/command-workspace--agent-notebook-process process)
      (setq td/command-workspace--agent-notebook-osc-fragment "")
      (setq td/command-workspace--agent-notebook-osc-active-p nil)
      (setq td/command-workspace--agent-notebook-output-block-open-p nil)
      (setq td/command-workspace--agent-notebook-blocked-permission-request-id nil)
      (setq td/command-workspace--agent-notebook-blocked-question-id nil)
      (process-put process 'td-agent-events nil)
      (td/command-workspace--agent-notebook-start-running-cell prompt))
    (message "Started td-agent task%s"
             (if (and task-title (not (string-empty-p task-title)))
                 (format " %s" task-title)
               ""))
    argv))

(defun td/command-workspace-agent-notebook-submit (&optional prompt)
  "Submit the current Draft Notebook Cell to td-agent.

When the notebook already has a Transcript Session id, submit via
non-interactive Agent Resume.  Otherwise start a new titled Agent Task."
  (interactive)
  (unless (derived-mode-p 'td/command-workspace-agent-notebook-mode)
    (user-error "Not in a td-agent notebook buffer"))
  (when (and td/command-workspace--agent-notebook-process
             (process-live-p td/command-workspace--agent-notebook-process))
    (user-error "td-agent is already running in this notebook"))
  (let* ((prompt (or prompt
                     (td/command-workspace--agent-notebook-draft-prompt)))
         (project-root (or td/command-workspace--agent-notebook-project-root
                           (td/command-workspace--current-root))))
    (when (string-empty-p prompt)
      (user-error "Draft Notebook Cell is empty"))
    (if td/command-workspace--agent-notebook-session-id
        (td/command-workspace--agent-resume
         `(("event" . "notebook_submit")
           ("session_id" . ,td/command-workspace--agent-notebook-session-id)
           ,@(when td/command-workspace--agent-notebook-project-root
               `(("cwd" .
                  ,td/command-workspace--agent-notebook-project-root))))
         prompt
         (current-buffer))
      (td/command-workspace--agent-run
       prompt
       project-root
       td/command-workspace--agent-notebook-task-title
       (current-buffer)))))

(defun td/command-workspace--agent-event-in-project-p (project-root event)
  "Return non-nil when transcript EVENT belongs under PROJECT-ROOT."
  (or (null project-root)
      (when-let* ((cwd (td/command-workspace--json-get "cwd" event))
                  (project-path (ignore-errors
                                  (file-truename
                                   (file-name-as-directory project-root))))
                  (event-path (ignore-errors
                                (file-truename
                                 (file-name-as-directory cwd)))))
        (string-prefix-p project-path event-path))))

(defun td/command-workspace--agent-read-recent-events (&optional project-root limit)
  "Return recent transcript events, optionally filtered by PROJECT-ROOT."
  (when (file-exists-p td/command-workspace-agent-transcript-log)
    (with-temp-buffer
      (insert-file-contents td/command-workspace-agent-transcript-log)
      (let ((lines (last (split-string (buffer-string) "\n" t)
                         (or limit 40))))
        (seq-filter
         (lambda (event)
           (td/command-workspace--agent-event-in-project-p project-root event))
         (seq-keep
          (lambda (line)
            (ignore-errors
              (json-parse-string line
                                 :object-type 'alist
                                 :array-type 'list
                                 :false-object nil
                                 :null-object nil)))
          lines))))))

(defun td/command-workspace--agent-read-session-events (session-id)
  "Return canonical transcript events for SESSION-ID."
  (when (and session-id
             (not (string-empty-p session-id))
             (file-exists-p td/command-workspace-agent-transcript-log))
    (with-temp-buffer
      (insert-file-contents td/command-workspace-agent-transcript-log)
      (seq-filter
       (lambda (event)
         (string-equal
          session-id
          (td/command-workspace--agent-event-session-id event)))
       (seq-keep
        (lambda (line)
          (ignore-errors
            (json-parse-string line
                               :object-type 'alist
                               :array-type 'list
                               :false-object nil
                               :null-object nil)))
        (split-string (buffer-string) "\n" t))))))

(defun td/command-workspace--agent-notebook-event-prompt (event)
  "Return EVENT's operator prompt text."
  (or (td/command-workspace--json-get-any '("query" "prompt" "message") event)
      ""))

(defun td/command-workspace--agent-notebook-event-response (event)
  "Return EVENT's user-visible response text."
  (or (td/command-workspace--json-get-any
       '("summary" "message" "response" "text")
       event)
      ""))

(defun td/command-workspace--agent-notebook-render-tool-activity (events)
  "Render tool activity from EVENTS collapsed by default."
  (let ((tool-events
         (seq-filter
          (lambda (event)
            (member (td/command-workspace--agent-event-type event)
                    '("tool_start" "tool_complete")))
          events)))
    (when tool-events
      (insert "\nTool Activity:\n"
              "# :state: collapsed\n\n")
      (dolist (event tool-events)
        (let ((tool-name (or (td/command-workspace--json-get "tool_name" event)
                             "tool"))
              (tool-input
               (td/command-workspace--agent-preview-string
                (td/command-workspace--json-get "tool_input" event)))
              (response
               (td/command-workspace--agent-notebook-event-response event)))
          (insert (format "- %s\n" tool-name))
          (let ((detail-start (point)))
            (unless (string-empty-p tool-input)
              (insert "  Input: " tool-input "\n"))
            (unless (string-empty-p response)
              (insert "  Output: " response "\n"))
            (add-text-properties detail-start (point)
                                 '(invisible td-agent-tool-activity))))))))

(defun td/command-workspace--agent-notebook-render-complete-cell
    (session-id prompt events stop-event)
  "Render one Complete Notebook Cell from canonical transcript EVENTS."
  (let ((summary
         (td/command-workspace--agent-notebook-event-response stop-event)))
    (insert "# %% Complete Notebook Cell\n"
            "# :state: complete\n")
    (when session-id
      (insert (format "# :session: %s\n" session-id)))
    (insert "\n" prompt "\n")
    (unless (string-empty-p summary)
      (insert "\nResponse:\n\n" summary "\n"))
    (td/command-workspace--agent-notebook-render-tool-activity events)
    (insert "\n")))

(defun td/command-workspace--agent-notebook-render-running-cell
    (session-id prompt events)
  "Render one Running Notebook Cell from canonical transcript EVENTS."
  (insert "# %% Running Notebook Cell\n"
          "# :state: running\n")
  (when session-id
    (insert (format "# :session: %s\n" session-id)))
  (insert "\n" prompt "\n")
  (td/command-workspace--agent-notebook-render-tool-activity events)
  (insert "\n"))

(defun td/command-workspace--agent-notebook-render-events (events)
  "Rebuild the current Agent Notebook projection from canonical EVENTS."
  (let* ((session-id
          (seq-some #'td/command-workspace--agent-event-session-id events))
         (project-root
          (or (seq-some #'td/command-workspace--agent-event-project-root events)
              td/command-workspace--agent-notebook-project-root
              default-directory))
         (task-title
          (or (seq-some #'td/command-workspace--agent-event-task-title events)
              td/command-workspace--agent-notebook-task-title))
         current-prompt
         current-events
         ready-for-draft)
    (setq-local td/command-workspace--agent-notebook-project-root
                (file-name-as-directory project-root))
    (setq-local td/command-workspace--agent-notebook-session-id session-id)
    (setq-local td/command-workspace--agent-notebook-task-title task-title)
    (when session-id
      (puthash session-id (current-buffer)
               td/command-workspace--agent-notebook-session-buffers))
    (td/command-workspace--agent-notebook-clear-draft)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (td/command-workspace--agent-notebook-insert-header project-root
                                                          task-title)
      (dolist (event events)
        (pcase (td/command-workspace--agent-event-type event)
          ("prompt_submit"
           (when current-prompt
             (td/command-workspace--agent-notebook-render-running-cell
              session-id current-prompt (nreverse current-events)))
           (setq current-prompt
                 (td/command-workspace--agent-notebook-event-prompt event))
           (setq current-events nil)
           (setq ready-for-draft nil))
          ("stop"
           (when current-prompt
             (td/command-workspace--agent-notebook-render-complete-cell
              session-id current-prompt (nreverse current-events) event))
           (setq current-prompt nil)
           (setq current-events nil))
          ("idle_prompt"
           (setq ready-for-draft t))
          (_
           (when current-prompt
             (push event current-events)))))
      (when current-prompt
        (td/command-workspace--agent-notebook-render-running-cell
         session-id current-prompt (nreverse current-events))))
    (when (or ready-for-draft (not current-prompt))
      (td/command-workspace--agent-notebook-insert-draft-cell))
    (td/command-workspace--agent-notebook-protect-buffer)))

(defun td/command-workspace--agent-notebook-reconcile-current-session ()
  "Rebuild the current notebook from the Canonical Event Log when possible."
  (when-let* ((session-id td/command-workspace--agent-notebook-session-id)
              (events
               (td/command-workspace--agent-read-session-events session-id)))
    (td/command-workspace--agent-notebook-render-events events)
    t))

(defun td/command-workspace-agent-notebook-refresh-transcript ()
  "Reconcile the current notebook from the Canonical Event Log."
  (interactive)
  (unless (derived-mode-p 'td/command-workspace-agent-notebook-mode)
    (user-error "Not in a td-agent notebook buffer"))
  (unless (file-exists-p td/command-workspace-agent-transcript-log)
    (user-error "No transcript log at %s"
                (abbreviate-file-name td/command-workspace-agent-transcript-log)))
  (if td/command-workspace--agent-notebook-session-id
      (let ((events
             (td/command-workspace--agent-read-session-events
              td/command-workspace--agent-notebook-session-id)))
        (unless events
          (user-error "No canonical events for Transcript Session %s"
                      td/command-workspace--agent-notebook-session-id))
        (td/command-workspace--agent-notebook-render-events events))
    (let ((events (td/command-workspace--agent-read-recent-events
                   td/command-workspace--agent-notebook-project-root
                   40)))
      (td/command-workspace--agent-notebook-insert "* Transcript Refresh\n\n")
      (let ((inhibit-read-only t))
        (dolist (event events)
          (td/command-workspace--render-agent-event event))))))

(defun td/command-workspace--agent-resume-argv (event prompt)
  "Return the Agent Resume argv for EVENT's session and PROMPT."
  (let ((session-id
         (td/command-workspace--agent-event-session-id event)))
    (unless (and session-id (not (string-empty-p session-id)))
      (user-error "Agent event has no Transcript Session id"))
    (list (td/command-workspace--agent-executable)
          td/command-workspace-agent-resume-command
          session-id
          prompt)))

(defun td/command-workspace--agent-resume (event prompt &optional buffer)
  "Invoke Agent Resume for EVENT's Transcript Session with PROMPT.

When BUFFER is live, render PROMPT as the next Notebook Cell and stream the
resume process into that buffer."
  (let* ((session-id
         (td/command-workspace--agent-event-session-id event))
         (project-root
          (td/command-workspace--agent-event-project-root event))
         (process-directory
          (if (and project-root (file-directory-p project-root))
              (file-name-as-directory project-root)
            default-directory))
         (argv
          (td/command-workspace--agent-resume-argv event prompt)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (let* ((default-directory process-directory)
                 (process
                  (make-process
                   :name "td-agent-resume"
                   :buffer buffer
                   :command argv
                   :connection-type 'pipe
                   :noquery t
                   :filter #'td/command-workspace--agent-notebook-insert-process-output
                   :sentinel #'td/command-workspace--agent-notebook-sentinel)))
            (setq td/command-workspace--agent-notebook-project-root
                  (or project-root
                      td/command-workspace--agent-notebook-project-root))
            (setq td/command-workspace--agent-notebook-session-id session-id)
            (setq td/command-workspace--agent-notebook-process process)
            (setq td/command-workspace--agent-notebook-osc-fragment "")
            (setq td/command-workspace--agent-notebook-osc-active-p nil)
            (td/command-workspace--agent-notebook-close-output-block)
            (setq td/command-workspace--agent-notebook-output-block-open-p nil)
            (setq td/command-workspace--agent-notebook-blocked-question-id nil)
            (process-put process 'td-agent-events nil)
            (td/command-workspace--agent-notebook-start-running-cell
             prompt session-id)))
      (let ((default-directory process-directory))
        (make-process
         :name "td-agent-resume"
         :buffer nil
         :command argv
         :connection-type 'pipe
         :noquery t)))
    (message "Sent td-agent resume for %s" session-id)
    argv))

(defun td/command-workspace--agent-permission-reply-argv (event decision)
  "Return the Permission Reply Command argv for EVENT and DECISION."
  (unless (member decision '("approve" "deny"))
    (user-error "Unknown permission decision: %s" decision))
  (let ((request-id
         (td/command-workspace--agent-permission-request-id event)))
    (unless (and request-id (not (string-empty-p request-id)))
      (user-error "Permission request event has no request id"))
    (append (list (td/command-workspace--agent-executable))
            td/command-workspace-agent-permission-reply-command
            (list request-id decision))))

(defun td/command-workspace--agent-permission-reply (event decision)
  "Invoke the Workflow Agent CLI Permission Reply Command for EVENT."
  (let* ((request-id
          (td/command-workspace--agent-permission-request-id event))
         (project-root
          (td/command-workspace--agent-event-project-root event))
         (default-directory
          (if (and project-root (file-directory-p project-root))
              (file-name-as-directory project-root)
            default-directory))
         (argv
          (td/command-workspace--agent-permission-reply-argv event decision)))
    (make-process
     :name "td-agent-permission-reply"
     :buffer nil
     :command argv
     :connection-type 'pipe
     :noquery t)
    (message "Sent td-agent permission %s for %s" decision request-id)
    argv))

(defun td/command-workspace-agent-action-approve ()
  "Approve the current Agent Action Transient permission request."
  (interactive)
  (unless td/command-workspace--agent-action-event
    (user-error "No agent action selected"))
  (unless (td/command-workspace--agent-action-permission-event-p)
    (user-error "Current agent action is not a permission request"))
  (td/command-workspace--agent-permission-reply
   td/command-workspace--agent-action-event
   "approve"))

(defun td/command-workspace-agent-action-deny ()
  "Deny the current Agent Action Transient permission request."
  (interactive)
  (unless td/command-workspace--agent-action-event
    (user-error "No agent action selected"))
  (unless (td/command-workspace--agent-action-permission-event-p)
    (user-error "Current agent action is not a permission request"))
  (td/command-workspace--agent-permission-reply
   td/command-workspace--agent-action-event
   "deny"))

(defun td/command-workspace--agent-mark-opened-session-read (event buffer)
  "Mark EVENT or BUFFER's Transcript Session notifications as read."
  (when-let* ((session-id
               (or (td/command-workspace--agent-event-session-id event)
                   (and (buffer-live-p buffer)
                        (with-current-buffer buffer
                          td/command-workspace--agent-notebook-session-id)))))
    (td/command-workspace--agent-notifications-mark-session-read session-id)))

(defun td/command-workspace--agent-action-open-notebook-buffer (event)
  "Open EVENT's Project Workspace and Agent Notebook, returning the buffer."
  (let* ((session-id
          (td/command-workspace--agent-event-session-id event))
         (buffer
          (and session-id
               (gethash session-id
                        td/command-workspace--agent-notebook-session-buffers)))
         (project-root
          (or (td/command-workspace--agent-event-project-root event)
              (and (buffer-live-p buffer)
                   (with-current-buffer buffer
                     td/command-workspace--agent-notebook-project-root)))))
    (cond
     ((buffer-live-p buffer)
      (when project-root
        (td/command-workspace-open-project-workspace project-root))
      (pop-to-buffer buffer)
      (td/command-workspace--agent-mark-opened-session-read event buffer)
      buffer)
     (project-root
      (td/command-workspace-open-project-workspace project-root)
      (let ((notebook-buffer
             (td/command-workspace--agent-notebook-buffer project-root)))
        (pop-to-buffer notebook-buffer)
        (td/command-workspace--agent-mark-opened-session-read
         event notebook-buffer)
        notebook-buffer))
     (t
      (td/command-workspace-open-agent-notebook)
      (td/command-workspace--agent-mark-opened-session-read
       event (current-buffer))
      (current-buffer)))))

(defun td/command-workspace-agent-action-answer (answer)
  "Answer the current Agent Action Transient question with ANSWER."
  (interactive (list (read-string "Answer: ")))
  (unless td/command-workspace--agent-action-event
    (user-error "No agent action selected"))
  (unless (td/command-workspace--agent-action-question-event-p)
    (user-error "Current agent action is not a question"))
  (prog1
      (td/command-workspace--agent-resume
       td/command-workspace--agent-action-event
       answer
       (td/command-workspace--agent-action-open-notebook-buffer
        td/command-workspace--agent-action-event))
    (td/command-workspace--agent-question-mark-answered
     td/command-workspace--agent-action-event)))

(defun td/command-workspace-agent-action-open-notebook ()
  "Open the notebook related to the current Agent Action Transient event."
  (interactive)
  (unless td/command-workspace--agent-action-event
    (user-error "No agent action selected"))
  (td/command-workspace--agent-action-open-notebook-buffer
   td/command-workspace--agent-action-event))

(defun td/command-workspace-agent-action-review-dismiss ()
  "Dismiss the current Agent Action Transient without sending a decision."
  (interactive)
  (unless td/command-workspace--agent-action-event
    (user-error "No agent action selected"))
  (td/command-workspace-agent-action-open-notebook)
  (message "Dismissed td-agent action for review")
  'dismissed)

(transient-define-prefix td/command-workspace--agent-action-transient-menu ()
  "Show actions for a pending CLI Agent Event."
  ["Agent Action"
   :description td/command-workspace--agent-action-transient-description
   ["Question"
    :if td/command-workspace--agent-action-question-event-p
    ("a" "Answer" td/command-workspace-agent-action-answer)]
   ["Permission"
    :if td/command-workspace--agent-action-permission-event-p
    ("a" "Approve" td/command-workspace-agent-action-approve)
    ("d" "Deny" td/command-workspace-agent-action-deny)]
   ["Review"
    ("o" "Open notebook" td/command-workspace-agent-action-open-notebook)
    ("r" "Review/dismiss" td/command-workspace-agent-action-review-dismiss)]])

(defun td/command-workspace-agent-action-transient (event)
  "Open an Agent Action Transient for actionable CLI Agent EVENT."
  (interactive
   (list
    (or td/command-workspace--agent-action-event
        (user-error "No agent action selected"))))
  (setq td/command-workspace--agent-action-event event)
  (td/command-workspace--agent-action-transient-menu))

(defvar td/command-workspace-agent-notification-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")
                #'td/command-workspace-agent-notification-activate)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for Emacs-local Agent Notification popups.")

(define-derived-mode td/command-workspace-agent-notification-mode special-mode
  "td-agent-notice"
  "Major mode for minimal Emacs-local Agent Notification popups.")

(defun td/command-workspace-agent-notification-activate (&optional id)
  "Activate Agent Notification ID, or the notification at point."
  (interactive)
  (let* ((id (or id td/command-workspace--agent-notification-current-id))
         (notification
          (and id
               (gethash id td/command-workspace--agent-notifications))))
    (unless notification
      (user-error "No Agent Notification selected"))
    (td/command-workspace--agent-action-queue-activate-item
     (td/command-workspace--agent-action-queue-item notification t))))

(defvar td/command-workspace-agent-action-queue-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")
                #'td/command-workspace-agent-action-queue-activate)
    (define-key map (kbd "g")
                #'td/command-workspace--agent-action-queue-refresh)
    (define-key map (kbd "f")
                #'td/command-workspace-agent-action-queue-set-filter)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for the Agent Action Queue.")

(define-derived-mode td/command-workspace-agent-action-queue-mode
  tabulated-list-mode "Agent Queue"
  "Major mode for the Agent Action Queue."
  (setq tabulated-list-format
        [("Kind" 12 t)
         ("State" 10 t)
         ("Session" 18 t)
         ("Summary" 0 t)])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook
            #'td/command-workspace--agent-action-queue-refresh
            nil t)
  (tabulated-list-init-header))

(defun td/command-workspace--agent-action-queue-entry (item)
  "Return a `tabulated-list-mode' entry for queue ITEM."
  (list
   (plist-get item :id)
   (vector
    (or (plist-get item :kind-label) "")
    (symbol-name (or (plist-get item :state) 'unknown))
    (or (plist-get item :session-id) "")
    (let ((title (or (plist-get item :title) ""))
          (summary (or (plist-get item :summary) "")))
      (if (string-empty-p summary)
          title
        (format "%s - %s" title summary))))))

(defun td/command-workspace--agent-action-queue-refresh ()
  "Refresh the Agent Action Queue buffer."
  (interactive)
  (setq tabulated-list-entries
        (mapcar #'td/command-workspace--agent-action-queue-entry
                (td/command-workspace--agent-action-queue-items
                 td/command-workspace--agent-action-queue-filter)))
  (tabulated-list-print t))

(defun td/command-workspace-open-agent-action-queue (&optional filter)
  "Open the Agent Action Queue, optionally narrowed by FILTER."
  (interactive)
  (let ((buffer (get-buffer-create "*td-agent action queue*")))
    (with-current-buffer buffer
      (td/command-workspace-agent-action-queue-mode)
      (setq-local td/command-workspace--agent-action-queue-filter filter)
      (td/command-workspace--agent-action-queue-refresh))
    (pop-to-buffer buffer)))

(defun td/command-workspace-agent-action-queue-set-filter (filter)
  "Set the Agent Action Queue FILTER."
  (interactive
   (list
    (let ((choice
           (completing-read
            "Queue filter: "
            '("all" "actionable" "unread" "permission" "question"
              "completed" "error")
            nil t nil nil "all")))
      (unless (string-equal choice "all")
        (intern choice)))))
  (setq-local td/command-workspace--agent-action-queue-filter filter)
  (td/command-workspace--agent-action-queue-refresh))

(defun td/command-workspace-agent-action-queue-activate (&optional id)
  "Activate Agent Action Queue item ID, or the item at point."
  (interactive)
  (let* ((id (or id (tabulated-list-get-id)))
         (notification
          (and id
               (gethash id td/command-workspace--agent-notifications))))
    (unless notification
      (user-error "No Agent Action Queue item selected"))
    (td/command-workspace--agent-action-queue-activate-item
     (td/command-workspace--agent-action-queue-item notification t))))

(define-key td/command-workspace-prefix-map (kbd "w")
            #'td/command-workspace-open-project-workspace)
(define-key td/command-workspace-prefix-map (kbd "t")
            #'td/command-workspace-open-project-terminal)
(define-key td/command-workspace-prefix-map (kbd "a")
            #'td/command-workspace-run-project-agent)
(define-key td/command-workspace-prefix-map (kbd "A")
            #'td/command-workspace-agent-auth-status)
(define-key td/command-workspace-prefix-map (kbd "n")
            #'td/command-workspace-open-agent-notebook)
(define-key td/command-workspace-prefix-map (kbd "N")
            #'td/command-workspace-new-agent-notebook)
(define-key td/command-workspace-prefix-map (kbd "l")
            #'td/command-workspace-open-agent-transcript)
(define-key td/command-workspace-prefix-map (kbd "q")
            #'td/command-workspace-open-agent-action-queue)
(define-key td/command-workspace-prefix-map (kbd "s")
            #'td/command-workspace-status)

(defun td/command-workspace-install (leader-map)
  "Attach command-workspace key bindings to LEADER-MAP."
  (define-key leader-map (kbd "w") td/command-workspace-prefix-map))

(provide 'td-command-workspace)
