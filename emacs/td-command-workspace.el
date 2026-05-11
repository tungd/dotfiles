;;; td-command-workspace.el --- Project workspaces + tmux-backed terminals -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'tab-bar)
(require 'json)
(require 'org)

(defvar eat-buffer-name)

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

When nil, prefer `td-agent' from PATH, then the built binary under
`td/command-workspace-agent-source-directory'."
  :type '(choice (const :tag "Auto" nil)
                 file)
  :group 'td/command-workspace)

(defcustom td/command-workspace-agent-transcript-log
  (expand-file-name "~/.td-agent/transcripts/events.jsonl")
  "Canonical JSONL transcript log written by the Workflow Agent CLI."
  :type 'file
  :group 'td/command-workspace)

(defvar td/command-workspace--session-buffers (make-hash-table :test #'equal))
(defvar-local td/command-workspace--has-attached-p nil)
(defvar-local td/command-workspace--agent-notebook-project-root nil)
(defvar-local td/command-workspace--agent-notebook-process nil)
(defvar-local td/command-workspace--agent-notebook-osc-fragment "")
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
      (executable-find "td-agent")
      (let ((built (td/command-workspace--agent-built-executable)))
        (when (file-executable-p built)
          built))
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

When PROJECT-ROOT is nil, use the current project. Outside a project, use the
current file's directory or `default-directory'. When FILE is non-nil, open it as
the workspace dashboard."
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
    (define-key map (kbd "g") #'td/command-workspace-agent-notebook-refresh-transcript)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for td-agent notebook buffers.")

(define-derived-mode td/command-workspace-agent-notebook-mode org-mode "td-agent"
  "Read-only project notebook for td-agent runs."
  (setq-local buffer-read-only t)
  (setq-local truncate-lines nil))

(defun td/command-workspace--agent-notebook-buffer-name (project-root)
  "Return notebook buffer name for PROJECT-ROOT."
  (format "*td-agent notebook: %s*"
          (file-name-nondirectory (directory-file-name project-root))))

(defun td/command-workspace--agent-notebook-buffer (project-root)
  "Return the notebook buffer for PROJECT-ROOT."
  (let ((buffer (get-buffer-create
                 (td/command-workspace--agent-notebook-buffer-name project-root))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'td/command-workspace-agent-notebook-mode)
        (td/command-workspace-agent-notebook-mode)
        (setq-local td/command-workspace--agent-notebook-project-root
                    (file-name-as-directory project-root))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "#+title: td-agent notebook\n")
          (insert (format "#+property: project %s\n\n"
                          (abbreviate-file-name project-root)))
          (insert "* Notes\n\n")
          (insert "Use =C-c C-c= to submit a prompt. Use =g= to refresh transcript events.\n\n"))))
    buffer))

(defun td/command-workspace-open-agent-notebook (&optional project-root)
  "Open the Emacs-native td-agent notebook for PROJECT-ROOT."
  (interactive)
  (let* ((project-root (or project-root (td/command-workspace--current-root)))
         (buffer (td/command-workspace--agent-notebook-buffer project-root)))
    (td/command-workspace-open-project-workspace project-root)
    (pop-to-buffer buffer)))

(defun td/command-workspace--agent-notebook-insert (&rest strings)
  "Insert STRINGS in the current read-only notebook buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (dolist (string strings)
      (insert string))
    (goto-char (point-max))))

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
      (process-put process 'td-agent-events
                   (append (process-get process 'td-agent-events)
                           (list event))))))

(defun td/command-workspace--agent-notebook-insert-process-output (process chunk)
  "Insert PROCESS output CHUNK, stripping td-agent OSC events."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let* ((data (concat td/command-workspace--agent-notebook-osc-fragment chunk))
             (start 0)
             (osc-regexp "\e]777;notify;warp://cli-agent;\\([^\a]*\\)\a"))
        (setq td/command-workspace--agent-notebook-osc-fragment "")
        (while (string-match osc-regexp data start)
          (let ((plain (substring data start (match-beginning 0)))
                (body (match-string 1 data)))
            (unless (string-empty-p plain)
              (td/command-workspace--agent-notebook-insert plain))
            (td/command-workspace--agent-notebook-handle-osc process body)
            (setq start (match-end 0))))
        (let ((tail (substring data start)))
          (if-let* ((osc-start (string-match "\e]777;notify;warp://cli-agent;" tail)))
              (progn
                (td/command-workspace--agent-notebook-insert
                 (substring tail 0 osc-start))
                (setq td/command-workspace--agent-notebook-osc-fragment
                      (substring tail osc-start)))
            (td/command-workspace--agent-notebook-insert tail)))))))

(defun td/command-workspace--agent-notebook-sentinel (process event)
  "Finalize notebook PROCESS after EVENT."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((events (process-get process 'td-agent-events))
            (status (process-exit-status process)))
        (td/command-workspace--agent-notebook-insert
         "\n#+end_example\n"
         (format "\n[td-agent exited %s: %s]\n\n"
                 status
                 (string-trim event)))
        (when events
          (td/command-workspace--agent-notebook-insert "* Events\n\n")
          (let ((inhibit-read-only t))
            (dolist (event events)
              (td/command-workspace--render-agent-event event))))
        (setq td/command-workspace--agent-notebook-process nil)))))

(defun td/command-workspace-agent-notebook-submit (prompt)
  "Submit PROMPT to td-agent from the current notebook buffer."
  (interactive (list (read-string "Agent prompt: ")))
  (unless (derived-mode-p 'td/command-workspace-agent-notebook-mode)
    (user-error "Not in a td-agent notebook buffer"))
  (when (and td/command-workspace--agent-notebook-process
             (process-live-p td/command-workspace--agent-notebook-process))
    (user-error "td-agent is already running in this notebook"))
  (let* ((project-root (or td/command-workspace--agent-notebook-project-root
                           (td/command-workspace--current-root)))
         (default-directory (file-name-as-directory project-root))
         (process
          (make-process
           :name "td-agent-notebook"
           :buffer (current-buffer)
           :command (list (td/command-workspace--agent-executable) "run" prompt)
           :connection-type 'pipe
           :noquery t
           :filter #'td/command-workspace--agent-notebook-insert-process-output
           :sentinel #'td/command-workspace--agent-notebook-sentinel)))
    (setq td/command-workspace--agent-notebook-project-root project-root)
    (setq td/command-workspace--agent-notebook-process process)
    (setq td/command-workspace--agent-notebook-osc-fragment "")
    (process-put process 'td-agent-events nil)
    (td/command-workspace--agent-notebook-insert
     (format "* Prompt  %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S"))
     "#+begin_quote\n"
     prompt
     "\n#+end_quote\n\n"
     "* Output\n\n"
     "#+begin_example\n")))

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

(defun td/command-workspace-agent-notebook-refresh-transcript ()
  "Append the latest transcript rendering to the current notebook."
  (interactive)
  (unless (derived-mode-p 'td/command-workspace-agent-notebook-mode)
    (user-error "Not in a td-agent notebook buffer"))
  (unless (file-exists-p td/command-workspace-agent-transcript-log)
    (user-error "No transcript log at %s"
                (abbreviate-file-name td/command-workspace-agent-transcript-log)))
  (let ((events (td/command-workspace--agent-read-recent-events
                 td/command-workspace--agent-notebook-project-root
                 40)))
    (td/command-workspace--agent-notebook-insert "* Transcript Refresh\n\n")
    (let ((inhibit-read-only t))
      (dolist (event events)
        (td/command-workspace--render-agent-event event)))))

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
(define-key td/command-workspace-prefix-map (kbd "l")
            #'td/command-workspace-open-agent-transcript)
(define-key td/command-workspace-prefix-map (kbd "s")
            #'td/command-workspace-status)

(defun td/command-workspace-install (leader-map)
  "Attach command-workspace key bindings to LEADER-MAP."
  (define-key leader-map (kbd "w") td/command-workspace-prefix-map))

(provide 'td-command-workspace)
