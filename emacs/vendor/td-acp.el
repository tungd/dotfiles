;;; td-acp.el --- Org-backed ACP UI on top of acp.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Tung Dao

;;; Commentary:
;;
;; `td-acp' provides a project-local ACP UI that depends on the MELPA
;; `acp' package for transport and protocol framing.
;;
;; Sessions are stored as Org files in:
;;
;;   <project-root>/.agents/sessions/<session-id>.org
;;
;; The primary interaction surface is a dedicated per-session prompt
;; buffer.  The transcript remains a separate Org buffer.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'map)
(require 'org)
(require 'project)
(require 'subr-x)
(require 'tabulated-list)

(require 'acp nil t)

(declare-function acp-make-client "acp" (&rest args))
(declare-function acp-make-authenticate-request "acp" (&rest args))
(declare-function acp-make-error "acp" (&rest args))
(declare-function acp-make-fs-read-text-file-response "acp" (&rest args))
(declare-function acp-make-fs-write-text-file-response "acp" (&rest args))
(declare-function acp-make-session-cancel-notification "acp" (&rest args))
(declare-function acp-make-session-load-request "acp" (&rest args))
(declare-function acp-make-session-new-request "acp" (&rest args))
(declare-function acp-make-session-request-permission-response "acp" (&rest args))
(declare-function acp-send-notification "acp" (&rest args))
(declare-function acp-send-request "acp" (&rest args))
(declare-function acp-send-response "acp" (&rest args))
(declare-function acp-shutdown "acp" (&rest args))
(declare-function acp-subscribe-to-errors "acp" (&rest args))
(declare-function acp-subscribe-to-notifications "acp" (&rest args))
(declare-function acp-subscribe-to-requests "acp" (&rest args))

(defgroup td-acp nil
  "ACP session UI backed by Org files."
  :group 'tools
  :prefix "td-acp-")

(defcustom td-acp-agent-program "claude-code-acp"
  "Program name or absolute path for the ACP agent."
  :type 'string
  :group 'td-acp)

(defcustom td-acp-agent-args nil
  "List of extra arguments passed to the ACP agent."
  :type '(repeat string)
  :group 'td-acp)

(defcustom td-acp-session-directory-name ".agents/sessions"
  "Project-relative directory used to store Org session transcripts."
  :type 'string
  :group 'td-acp)

(defcustom td-acp-prompt-buffer-name-format "*td-acp:%p:%s*"
  "Prompt buffer name format.

The format string accepts:
- `%p' for the project name
- `%s' for the session id or `pending'"
  :type 'string
  :group 'td-acp)

(defcustom td-acp-prompt-window-height 8
  "Height of the dedicated bottom prompt window."
  :type 'integer
  :group 'td-acp)

(defcustom td-acp-permission-policy 'ask-risky
  "How `td-acp' should handle ACP permission requests.

Supported values:
- `ask' asks for every request.
- `ask-risky' auto-allows obviously safe requests and asks for risky ones.
- `allow-all' accepts the first allow option automatically.
- `deny-all' accepts the first reject option automatically.
- a function called with REQUEST and SESSION, returning an option id string
  or the symbol `cancel'."
  :type '(choice (const :tag "Ask for everything" ask)
                 (const :tag "Ask for risky requests" ask-risky)
                 (const :tag "Allow all" allow-all)
                 (const :tag "Deny all" deny-all)
                 (function :tag "Custom resolver"))
  :group 'td-acp)

(defcustom td-acp-client-info
  '((name . "td-acp")
    (title . "Tung ACP")
    (version . "0.1"))
  "Client info advertised during ACP initialization."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'td-acp)

(defcustom td-acp-auth-method-id nil
  "Optional ACP authentication method id to use after initialize."
  :type '(choice (const :tag "No explicit authenticate step" nil)
                 (string :tag "Method id"))
  :group 'td-acp)

(defcustom td-acp-auth-method nil
  "Optional ACP authentication payload sent with `td-acp-auth-method-id'."
  :type '(choice (const :tag "No auth payload" nil)
                 (alist :key-type symbol :value-type sexp))
  :group 'td-acp)

(defvar td-acp--sessions (make-hash-table :test #'equal)
  "Live sessions keyed by session id.")

(defvar td-acp--pending-sessions nil
  "Live sessions that do not yet have a session id.")

(defvar td-acp--terminal-counter 0)

(defvar-local td-acp--session nil
  "Current prompt buffer session.")

(defvar-local td-acp--session-list-root nil
  "Project root displayed by the current session list buffer.")

(defvar-local td-acp--session-list-metadata nil
  "Session metadata alist for the current list buffer.")

(cl-defstruct td-acp-session
  id
  title
  project-root
  transcript-file
  prompt-buffer
  transcript-buffer
  client
  agent-command
  created-at
  updated-at
  status
  turns
  load-session-capable-p
  available-commands
  current-mode-id
  config-options
  usage
  pending-user-echo
  pending-permission-ids
  live-p
  last-error)

(cl-defstruct td-acp-turn
  user
  assistant
  thoughts
  tool-calls
  permissions
  terminal-events
  unknown-updates
  prompt-response
  created-at
  updated-at)

(cl-defstruct td-acp-terminal
  id
  session-id
  process
  command
  args
  cwd
  output
  output-byte-limit
  exit-code
  signal
  released-p
  waiters)

(defvar td-acp--terminals (make-hash-table :test #'equal)
  "Known ACP terminals keyed by terminal id.")

(defun td-acp--assert-acp ()
  "Ensure the `acp' package is available."
  (unless (and (fboundp 'acp-make-client)
               (fboundp 'acp-send-request))
    (user-error "The MELPA package `acp' is required")))

(defun td-acp--now ()
  "Return the current timestamp in ISO 8601 format."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun td-acp--field (object key)
  "Return KEY from OBJECT, handling both symbol and keyword variants."
  (or (map-elt object key)
      (map-elt object (intern (format ":%s" key)))))

(defun td-acp--set-field (object key value)
  "Set KEY to VALUE in OBJECT."
  (let ((slot (if (map-contains-key object key)
                  key
                (intern (format ":%s" key)))))
    (map-put! object slot value)))

(defun td-acp--project-root ()
  "Return the current project root."
  (if-let ((project (project-current nil)))
      (directory-file-name (expand-file-name (project-root project)))
    (user-error "Not in a project")))

(defun td-acp--project-name (project-root)
  "Return a display name for PROJECT-ROOT."
  (file-name-nondirectory (directory-file-name project-root)))

(defun td-acp--sessions-dir (project-root)
  "Return the session directory for PROJECT-ROOT."
  (expand-file-name td-acp-session-directory-name project-root))

(defun td-acp--ensure-sessions-dir (project-root)
  "Ensure PROJECT-ROOT session directory exists."
  (make-directory (td-acp--sessions-dir project-root) t))

(defun td-acp--transcript-file (project-root session-id)
  "Return transcript path for PROJECT-ROOT and SESSION-ID."
  (expand-file-name (format "%s.org" session-id)
                    (td-acp--sessions-dir project-root)))

(defun td-acp--format-spec (format-string project-root session-id)
  "Format FORMAT-STRING with PROJECT-ROOT and SESSION-ID."
  (format-spec format-string
               `((?p . ,(td-acp--project-name project-root))
                 (?s . ,(or session-id "pending")))))

(defun td-acp--command-string ()
  "Return the configured ACP command as a single display string."
  (string-join (cons td-acp-agent-program td-acp-agent-args) " "))

(defun td-acp--register-session (session)
  "Register SESSION in the live session tables."
  (if-let ((session-id (td-acp-session-id session)))
      (puthash session-id session td-acp--sessions)
    (cl-pushnew session td-acp--pending-sessions :test #'eq)))

(defun td-acp--unregister-pending-session (session)
  "Remove SESSION from pending live sessions."
  (setq td-acp--pending-sessions (delq session td-acp--pending-sessions)))

(defun td-acp--lookup-session (session-id)
  "Return live session for SESSION-ID."
  (gethash session-id td-acp--sessions))

(defun td-acp--put-session (session)
  "Store SESSION after it gets a session id."
  (td-acp--unregister-pending-session session)
  (when-let ((session-id (td-acp-session-id session)))
    (puthash session-id session td-acp--sessions)))

(defun td-acp--remove-session (session)
  "Remove SESSION from the live registry."
  (td-acp--unregister-pending-session session)
  (when-let ((session-id (td-acp-session-id session)))
    (remhash session-id td-acp--sessions)))

(defun td-acp--make-turn (&optional user-text)
  "Create a new turn optionally seeded with USER-TEXT."
  (make-td-acp-turn :user user-text
                    :assistant ""
                    :thoughts ""
                    :tool-calls nil
                    :permissions nil
                    :terminal-events nil
                    :unknown-updates nil
                    :created-at (td-acp--now)
                    :updated-at (td-acp--now)))

(defun td-acp--turn-list (session)
  "Return SESSION turns."
  (or (td-acp-session-turns session)
      (setf (td-acp-session-turns session) nil)))

(defun td-acp--set-session-updated (session)
  "Update SESSION timestamp."
  (setf (td-acp-session-updated-at session) (td-acp--now)))

(defun td-acp--current-turn (session &optional create)
  "Return SESSION's current turn.

When CREATE is non-nil, create the first turn if needed."
  (let ((turns (td-acp--turn-list session)))
    (cond
     (turns (car (last turns)))
     (create
      (let ((turn (td-acp--make-turn)))
        (setf (td-acp-session-turns session) (list turn))
        turn))
     (t nil))))

(defun td-acp--append-turn (session turn)
  "Append TURN to SESSION."
  (setf (td-acp-session-turns session)
        (append (td-acp--turn-list session) (list turn)))
  (td-acp--set-session-updated session)
  turn)

(defun td-acp--ensure-prompt-buffer (session)
  "Return prompt buffer for SESSION, creating it if necessary."
  (let* ((project-root (td-acp-session-project-root session))
         (buffer-name (td-acp--format-spec td-acp-prompt-buffer-name-format
                                           project-root
                                           (td-acp-session-id session)))
         (buffer (or (and (buffer-live-p (td-acp-session-prompt-buffer session))
                          (td-acp-session-prompt-buffer session))
                     (get-buffer-create buffer-name))))
    (with-current-buffer buffer
      (td-acp-prompt-mode)
      (setq-local td-acp--session session)
      (setq header-line-format '(:eval (td-acp--prompt-header-line td-acp--session))))
    (setf (td-acp-session-prompt-buffer session) buffer)
    buffer))

(defun td-acp--rename-prompt-buffer (session)
  "Rename SESSION's prompt buffer to match its final session id."
  (when-let ((buffer (td-acp-session-prompt-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (rename-buffer
         (td-acp--format-spec td-acp-prompt-buffer-name-format
                              (td-acp-session-project-root session)
                              (td-acp-session-id session))
         t)))))

(defun td-acp--prompt-header-line (session)
  "Return header line for SESSION."
  (format " ACP %s  |  session %s  |  %s  |  %s"
          (or (td-acp-session-status session) "idle")
          (or (td-acp-session-id session) "pending")
          (or (td-acp-session-agent-command session) (td-acp--command-string))
          (or (td-acp-session-transcript-file session) "transcript pending")))

(defun td-acp--ensure-transcript-file (session)
  "Ensure SESSION has a transcript file assigned."
  (unless (td-acp-session-transcript-file session)
    (when-let ((session-id (td-acp-session-id session)))
      (td-acp--ensure-sessions-dir (td-acp-session-project-root session))
      (setf (td-acp-session-transcript-file session)
            (td-acp--transcript-file (td-acp-session-project-root session)
                                     session-id))))
  (td-acp-session-transcript-file session))

(defun td-acp--quote-org (string)
  "Return STRING quoted for plain Org text."
  (replace-regexp-in-string "^\\([*#]\\)" ",\\1" (or string "")))

(defun td-acp--json-string (value)
  "Return VALUE pretty printed as JSON."
  (let ((json-encoding-pretty-print t))
    (json-serialize value)))

(defun td-acp--org-src-block (language content)
  "Return an Org source block with LANGUAGE and CONTENT."
  (format "#+begin_src %s\n%s\n#+end_src\n"
          language
          (string-trim-right (or content ""))))

(defun td-acp--org-heading-text (text)
  "Return TEXT escaped for use in an Org heading."
  (replace-regexp-in-string "[\r\n]+" " " (string-trim (or text ""))))

(defun td-acp--org-body-text (text)
  "Return TEXT escaped for use as Org body content."
  (let ((body (string-trim-right (or text ""))))
    (replace-regexp-in-string "^\\*" ",*" body)))

(defun td-acp--single-line-text-p (text)
  "Return non-nil when TEXT is a single logical line."
  (not (string-match-p "\n" (or text ""))))

(defun td-acp--format-session-metadata (session)
  "Return Org metadata block for SESSION."
  (string-join
   (delq nil
         (list (format "#+TITLE: %s" (or (td-acp-session-title session)
                                         (td-acp-session-id session)
                                         "ACP Session"))
               "#+STARTUP: showall"
               (format "#+PROPERTY: TD_ACP_SESSION_ID %s"
                       (or (td-acp-session-id session) ""))
               (format "#+PROPERTY: TD_ACP_PROJECT_ROOT %s"
                       (or (td-acp-session-project-root session) ""))
               (format "#+PROPERTY: TD_ACP_AGENT_COMMAND %s"
                       (or (td-acp-session-agent-command session) ""))
               (format "#+PROPERTY: TD_ACP_CREATED_AT %s"
                       (or (td-acp-session-created-at session) ""))
               (format "#+PROPERTY: TD_ACP_UPDATED_AT %s"
                       (or (td-acp-session-updated-at session) ""))
               (format "#+PROPERTY: TD_ACP_STATUS %s"
                       (or (td-acp-session-status session) ""))
               (format "#+PROPERTY: TD_ACP_LOAD_SESSION %s"
                       (if (td-acp-session-load-session-capable-p session) "true" "false"))
               (when (td-acp-session-current-mode-id session)
                 (format "#+PROPERTY: TD_ACP_CURRENT_MODE %s"
                         (td-acp-session-current-mode-id session)))))
   "\n"))

(defun td-acp--render-location (location)
  "Render ACP tool LOCATION into Org text."
  (let ((path (or (td-acp--field location 'path) ""))
        (line (td-acp--field location 'line)))
    (if line
        (format "- %s:%s" path line)
      (format "- %s" path))))

(defun td-acp--render-tool-call-block (label language content)
  "Render a labeled tool-call block using LANGUAGE and CONTENT."
  (concat (format "%s\n" label)
          (td-acp--org-src-block language content)))

(defun td-acp--render-inline-block (label language content)
  "Render LABEL and a source block with LANGUAGE and CONTENT."
  (when (and content (not (string-empty-p (string-trim (format "%s" content)))))
    (concat (format "%s\n" label)
            (td-acp--org-src-block language content)
            "\n")))

(defun td-acp--render-tool-call-content (content)
  "Render tool CONTENT into Org text."
  (let ((kind (td-acp--field content 'type)))
    (cond
     ((string= kind "content")
      (td-acp--render-tool-call-block
       "Content:"
       "text"
       (td-acp--render-content (td-acp--field content 'content))))
     ((string= kind "diff")
      (td-acp--render-tool-call-block
       "Diff:"
       "diff"
       (or (td-acp--field content 'diff)
           (td-acp--field content 'content)
           (td-acp--json-string content))))
     ((string= kind "terminal")
      (let* ((terminal-id (or (td-acp--field content 'terminalId)
                              (td-acp--field content 'terminal_id)))
             (terminal (and terminal-id (gethash terminal-id td-acp--terminals))))
        (concat (format "Terminal: %s\n" terminal-id)
                (when terminal
                  (td-acp--org-src-block "text"
                                         (or (td-acp-terminal-output terminal) ""))))))
     (t
      (td-acp--render-tool-call-block
       "Payload:"
       "json"
       (td-acp--json-string content))))))

(defun td-acp--tool-call-command (tool-call)
  "Return command string for TOOL-CALL, when present."
  (let ((raw-input (td-acp--field tool-call 'rawInput)))
    (or (td-acp--field raw-input 'command)
        (td-acp--field raw-input 'cmd))))

(defun td-acp--task-tool-call-p (tool-call)
  "Return non-nil when TOOL-CALL should be grouped under Tasks."
  (member (or (td-acp--field tool-call 'kind) "")
          '("think" "task" "plan")))

(defun td-acp--tool-call-heading (tool-call)
  "Return heading title for TOOL-CALL."
  (let ((command (td-acp--tool-call-command tool-call))
        (title (td-acp--field tool-call 'title))
        (raw-input (td-acp--field tool-call 'rawInput)))
    (cond
     ((and command (not (string-empty-p (string-trim command))))
      (format "`%s`" command))
     ((and title (not (string-empty-p (string-trim title))))
      title)
     ((td-acp--field raw-input 'description))
     ((td-acp--field tool-call 'kind))
     (t "Tool call"))))

(defun td-acp--render-tool-call (tool-call &optional level)
  "Render TOOL-CALL into Org at heading LEVEL."
  (let* ((title (or (td-acp--field tool-call 'title) "Tool call"))
         (heading-level (or level 2))
         (stars (make-string heading-level ?*))
         (tool-call-id (or (td-acp--field tool-call 'toolCallId)
                           (td-acp--field tool-call 'tool_call_id)))
         (status (or (td-acp--field tool-call 'status) ""))
         (kind (or (td-acp--field tool-call 'kind) ""))
         (locations (or (td-acp--field tool-call 'locations) nil))
         (content (or (td-acp--field tool-call 'content) nil))
         (raw-input (td-acp--field tool-call 'rawInput))
         (raw-output (td-acp--field tool-call 'rawOutput)))
    (concat
     (format "%s %s\n" stars (td-acp--org-heading-text (td-acp--tool-call-heading tool-call)))
     (when (or tool-call-id status kind)
       (concat
        ":PROPERTIES:\n"
        (when tool-call-id
          (format ":TOOL_CALL_ID: %s\n" tool-call-id))
        (when (and status (not (string-empty-p status)))
          (format ":STATUS: %s\n" status))
        (when (and kind (not (string-empty-p kind)))
          (format ":KIND: %s\n" kind))
        ":END:\n"))
     (when locations
       (concat "Affected locations:\n"
               (string-join (mapcar #'td-acp--render-location locations) "\n")
               "\n\n"))
     (when content
       (concat (mapconcat #'td-acp--render-tool-call-content content "\n") "\n"))
     (when raw-input
       (td-acp--render-tool-call-block
        "Raw input:"
        "json"
        (td-acp--json-string raw-input)))
     (when raw-output
       (td-acp--render-tool-call-block
        "Raw output:"
        "json"
        (td-acp--json-string raw-output)))
     "\n")))

(defun td-acp--render-permission (permission)
  "Render PERMISSION event into Org."
  (td-acp--render-inline-block
   (format "Permission: %s" (or (plist-get permission :summary) "Permission request"))
   "json"
   (td-acp--json-string (plist-get permission :raw))))

(defun td-acp--render-terminal-event (event)
  "Render terminal EVENT into Org."
  (td-acp--render-inline-block
   (format "Terminal: %s" (or (plist-get event :summary) "Terminal event"))
   "json"
   (td-acp--json-string (plist-get event :raw))))

(defun td-acp--render-content (content)
  "Render ACP CONTENT into Org text."
  (let ((content-type (td-acp--field content 'type)))
    (cond
     ((stringp content)
      (td-acp--quote-org content))
     ((string= content-type "text")
      (td-acp--quote-org (or (td-acp--field content 'text) "")))
     ((string= content-type "resource_link")
      (format "[[%s][%s]]"
              (or (td-acp--field content 'uri) "")
              (or (td-acp--field content 'name)
                  (td-acp--field content 'title)
                  (td-acp--field content 'uri)
                  "resource")))
     ((string= content-type "resource")
      (or (td-acp--field content 'text)
          (td-acp--json-string content)))
     (t
      (td-acp--json-string content)))))

(defun td-acp--render-user-section (turn index)
  "Render user-facing section for TURN and INDEX."
  (let* ((user-text (or (td-acp-turn-user turn) ""))
         (heading (if (td-acp--single-line-text-p user-text)
                      (format "* User: %s" (td-acp--org-heading-text user-text))
                    "* User"))
         (body (unless (td-acp--single-line-text-p user-text)
                 (td-acp--org-body-text user-text))))
    (concat heading
            "\n"
            (when body
              (concat body "\n"))
            "\n")))

(defun td-acp--render-agent-section (turn index)
  "Render agent-facing section for TURN and INDEX."
  (let* ((tool-calls (td-acp-turn-tool-calls turn))
         (tasks (cl-remove-if-not #'td-acp--task-tool-call-p tool-calls))
         (non-task-tool-calls (cl-remove-if #'td-acp--task-tool-call-p tool-calls)))
    (concat
     "* Agent\n"
     (let ((assistant-text (td-acp--org-body-text (or (td-acp-turn-assistant turn) ""))))
       (if (string-empty-p (string-trim assistant-text))
           ""
         (concat assistant-text "\n\n")))
     (let ((thoughts (string-trim (or (td-acp-turn-thoughts turn) ""))))
       (unless (string-empty-p thoughts)
         (concat "** Thoughts\n"
                 (td-acp--org-body-text thoughts)
                 "\n\n")))
     (when tasks
       (concat
        "** Tasks\n"
        (mapconcat (lambda (tool-call)
                     (td-acp--render-tool-call tool-call 3))
                   tasks
                   "\n")
        "\n"))
     (when non-task-tool-calls
       (concat
        "** Tool calls\n"
        (mapconcat (lambda (tool-call)
                     (td-acp--render-tool-call tool-call 3))
                   non-task-tool-calls
                   "\n")
        "\n"))
     (when (td-acp-turn-permissions turn)
       (concat "** Permissions\n"
               (mapconcat #'td-acp--render-permission
                          (td-acp-turn-permissions turn)
                          "\n")
               "\n"))
     (when (td-acp-turn-terminal-events turn)
       (concat "** Terminal\n"
               (mapconcat #'td-acp--render-terminal-event
                          (td-acp-turn-terminal-events turn)
                          "\n")
               "\n"))
     (when-let ((prompt-response (td-acp-turn-prompt-response turn)))
       (concat "** Prompt response\n"
               (td-acp--org-src-block "json"
                                      (td-acp--json-string prompt-response))
               "\n"))
     (when (td-acp-turn-unknown-updates turn)
       (concat "** Raw updates\n"
               (mapconcat (lambda (update)
                            (td-acp--render-inline-block
                             "Raw update:"
                             "json"
                             (td-acp--json-string update)))
                          (td-acp-turn-unknown-updates turn)
                          "\n")
               "\n")))))

(defun td-acp--render-turn (turn index)
  "Render TURN number INDEX into Org."
  (concat
   (td-acp--render-user-section turn index)
   (td-acp--render-agent-section turn index)))

(defun td-acp--session-org (session)
  "Render SESSION to an Org string."
  (concat
   (td-acp--format-session-metadata session)
   "\n\n"
   (when (td-acp-session-available-commands session)
     (concat "* Session State\n"
             (when (td-acp-session-current-mode-id session)
               (format "- Current mode: %s\n" (td-acp-session-current-mode-id session)))
             (format "- Available commands: %s\n"
                     (string-join
                      (mapcar (lambda (command)
                                (or (td-acp--field command 'name)
                                    (td-acp--field command 'command)
                                    (format "%s" command)))
                              (td-acp-session-available-commands session))
                      ", "))
             (when (td-acp-session-usage session)
               (td-acp--org-src-block "json"
                                      (td-acp--json-string (td-acp-session-usage session))))
             "\n"))
   (if (td-acp-session-turns session)
       (mapconcat (lambda (cell)
                    (td-acp--render-turn (cdr cell) (car cell)))
                  (cl-mapcar #'cons
                             (number-sequence 1 (length (td-acp-session-turns session)))
                             (td-acp-session-turns session))
                  "\n")
     (td-acp--render-turn (td-acp--make-turn) 1))))

(defun td-acp--persist-session (session)
  "Write SESSION transcript to disk."
  (when-let ((file (td-acp--ensure-transcript-file session)))
    (when (or (td-acp-session-turns session)
              (not (file-exists-p file)))
      (td-acp--ensure-sessions-dir (td-acp-session-project-root session))
      (let ((content (td-acp--session-org session))
            (buffer (and (buffer-live-p (td-acp-session-transcript-buffer session))
                         (td-acp-session-transcript-buffer session)))
            (following (td-acp--following-windows
                        (td-acp-session-transcript-buffer session))))
        (if (and buffer (equal (buffer-file-name buffer) file))
            (with-current-buffer buffer
              (let ((inhibit-read-only t)
                    (require-final-newline nil))
                (set-visited-file-modtime
                 (file-attribute-modification-time (file-attributes file)))
                (erase-buffer)
                (insert content)
                (write-region (point-min) (point-max) file nil 'silent)
                (set-buffer-modified-p nil)
                (org-mode)))
          (with-temp-file file
            (insert content)))
        (when buffer
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (set-buffer-modified-p nil)
              (org-mode)
              (td-acp--fold-detail-blocks buffer))))
        (td-acp--follow-transcript-buffer session following)
        (td-acp--refresh-session-list-buffers)))))

(defun td-acp--fold-detail-blocks (buffer)
  "Fold thought and tool-call subtrees in transcript BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (goto-char (point-min))
        (while (re-search-forward "^\\*+ " nil t)
          (beginning-of-line)
          (let* ((element (org-element-at-point))
                 (title (org-element-property :raw-value element))
                 (level (org-element-property :level element))
                 (should-fold
                  (cond
                   ((and (= level 2) (string= title "Thoughts"))
                    t)
                   ((= level 3)
                    (save-excursion
                      (and (org-up-heading-safe)
                           (member (org-get-heading t t t t)
                                   '("Tasks" "Tool calls")))))
                   (t nil))))
            (when should-fold
              (org-cycle-hide-drawers 'children)
              (outline-hide-subtree)))
          (forward-line 1))))))

(defun td-acp--open-transcript-buffer (session)
  "Return transcript buffer for SESSION."
  (when-let ((file (td-acp--ensure-transcript-file session)))
    (let ((buffer
           (or (and (buffer-live-p (td-acp-session-transcript-buffer session))
                    (td-acp-session-transcript-buffer session))
               (find-file-noselect file))))
      (with-current-buffer buffer
        (org-mode)
        (setq buffer-read-only t))
      (setf (td-acp-session-transcript-buffer session) buffer)
      (td-acp--fold-detail-blocks buffer)
      (with-current-buffer buffer
        (setq-local window-point-insertion-type t))
      buffer)))

(defun td-acp--following-windows (buffer)
  "Return BUFFER windows that are currently following output."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((max-pos (point-max)))
        (cl-remove-if-not
         (lambda (window)
           (and (window-live-p window)
                (>= (window-point window) (max (point-min) (- max-pos 2)))))
         (get-buffer-window-list buffer nil t))))))

(defun td-acp--follow-transcript-buffer (session &optional windows)
  "Scroll transcript WINDOWS for SESSION to the end.

When WINDOWS is nil, all visible transcript windows are updated."
  (when-let ((buffer (td-acp-session-transcript-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (dolist (window (or windows (get-buffer-window-list buffer nil t)))
          (when (window-live-p window)
            (set-window-point window (point-max))
            (with-selected-window window
              (goto-char (point-max))
              (recenter -1))))))))

(defun td-acp--window-can-split-for-prompt-p (window)
  "Return non-nil if WINDOW can be split into transcript and prompt panes."
  (>= (window-total-height window)
      (* 2 window-min-height)))

(defun td-acp--prompt-height-for-window (window)
  "Return prompt pane height to use when splitting WINDOW."
  (let* ((window-height (window-total-height window))
         (max-prompt-height (- window-height window-min-height)))
    (max window-min-height
         (min td-acp-prompt-window-height
              max-prompt-height))))

(defun td-acp--windows-by-height (&optional windows)
  "Return live WINDOWS sorted by descending height.

When WINDOWS is nil, use all non-minibuffer windows in the selected frame."
  (sort (cl-remove-if-not #'window-live-p
                          (copy-sequence (or windows (window-list nil 'no-mini))))
        (lambda (left right)
          (> (window-total-height left)
             (window-total-height right)))))

(defun td-acp--window-with-most-height (&optional windows)
  "Return the tallest window from WINDOWS."
  (car (td-acp--windows-by-height windows)))

(defun td-acp--best-display-window (&optional preferred)
  "Return best window for transcript plus prompt.

Use PREFERRED when it can be split, else pick the tallest splittable
window in the frame. Fall back to PREFERRED or selected window."
  (or (and preferred
           (window-live-p preferred)
           (td-acp--window-can-split-for-prompt-p preferred)
           preferred)
      (cl-find-if #'td-acp--window-can-split-for-prompt-p
                  (td-acp--windows-by-height))
      preferred
      (selected-window)))

(defun td-acp--preferred-display-window (transcript-wins prompt-wins selected)
  "Return preferred base window for displaying a SESSION pair."
  (cond
   ((and prompt-wins (not transcript-wins)
         (not (memq selected prompt-wins))
         (td-acp--window-can-split-for-prompt-p selected))
    selected)
   (transcript-wins (td-acp--window-with-most-height transcript-wins))
   (prompt-wins (td-acp--window-with-most-height prompt-wins))
   (t selected)))

(defun td-acp--delete-extra-prompt-windows (prompt-wins target)
  "Delete windows in PROMPT-WINS except TARGET."
  (dolist (window prompt-wins)
    (unless (eq window target)
      (ignore-errors (delete-window window)))))

(defun td-acp--paired-prompt-window (transcript-win prompt-buf)
  "Return prompt window below TRANSCRIPT-WIN showing PROMPT-BUF, or nil."
  (when (window-live-p transcript-win)
    (let ((below (window-in-direction 'below transcript-win)))
      (and below
           (eq (window-buffer below) prompt-buf)
           below))))

(defun td-acp--best-prompt-window (transcript-buf prompt-buf)
  "Return best visible window for PROMPT-BUF in current frame."
  (let* ((prompt-wins (get-buffer-window-list prompt-buf nil))
         (selected (selected-window))
         (selected-transcript-win (and (eq (window-buffer selected) transcript-buf)
                                       selected)))
    (or (td-acp--paired-prompt-window selected-transcript-win prompt-buf)
        (and (memq selected prompt-wins) selected)
        (td-acp--window-with-most-height prompt-wins))))

(defun td-acp--focus-prompt-window (transcript-buf prompt-buf)
  "Select a visible PROMPT-BUF window for the TRANSCRIPT-BUF session."
  (when-let ((window (td-acp--best-prompt-window transcript-buf prompt-buf)))
    (select-window window)))

(defun td-acp--display-session (session)
  "Display prompt and transcript buffers for SESSION."
  (let ((prompt (td-acp--ensure-prompt-buffer session))
        (transcript (td-acp--open-transcript-buffer session)))
    (if (not transcript)
        (pop-to-buffer-same-window prompt)
      (let* ((transcript-wins (get-buffer-window-list transcript nil))
             (prompt-wins (get-buffer-window-list prompt nil))
             (selected (selected-window)))
        (if (and transcript-wins prompt-wins)
            (td-acp--focus-prompt-window transcript prompt)
          (let* ((preferred (td-acp--preferred-display-window
                             transcript-wins prompt-wins selected))
                 (target (td-acp--best-display-window preferred))
                 (prompt-win nil))
            (when (and prompt-wins (not transcript-wins))
              (td-acp--delete-extra-prompt-windows prompt-wins target))
            (with-selected-window target
              (unless (td-acp--window-can-split-for-prompt-p target)
                (delete-other-windows target))
              (unless (td-acp--window-can-split-for-prompt-p target)
                (user-error "Window too small for transcript + prompt layout"))
              (switch-to-buffer transcript)
              (with-current-buffer transcript
                (goto-char (point-max)))
              (let ((prompt-height (td-acp--prompt-height-for-window target)))
                (setq prompt-win (split-window nil (- prompt-height) 'below))
                (set-window-buffer prompt-win prompt)))
            (when (window-live-p prompt-win)
              (select-window prompt-win))))
        (td-acp--follow-transcript-buffer session)))))

(defun td-acp--append-text (old text)
  "Append TEXT to OLD, handling nil values."
  (concat (or old "") (or text "")))

(defun td-acp--content->text (update)
  "Extract readable text from session UPDATE."
  (let ((content (td-acp--field update 'content)))
    (string-trim-right
     (cond
      ((vectorp content)
       (mapconcat #'td-acp--render-content (append content nil) ""))
      ((listp content)
       (td-acp--render-content content))
      (t (format "%s" content))))))

(defun td-acp--find-tool-call (turn tool-call-id)
  "Return tool call in TURN identified by TOOL-CALL-ID."
  (cl-find-if
   (lambda (tool-call)
     (equal (or (td-acp--field tool-call 'toolCallId)
                (td-acp--field tool-call 'tool_call_id))
            tool-call-id))
   (td-acp-turn-tool-calls turn)))

(defun td-acp--merge-tool-call (existing update)
  "Merge ACP tool UPDATE into EXISTING."
  (dolist (key '(title kind status content locations))
    (when-let ((value (td-acp--field update key)))
      (td-acp--set-field existing key value)))
  (dolist (key '(rawInput rawOutput))
    (when (map-contains-key update key)
      (td-acp--set-field existing key (td-acp--field update key))))
  existing)

(defun td-acp--record-permission-event (session request raw-request outcome)
  "Append permission event for SESSION.

REQUEST is the incoming ACP request.  RAW-REQUEST and OUTCOME are stored for
transcript rendering."
  (let* ((turn (or (td-acp--current-turn session t)
                   (td-acp--append-turn session (td-acp--make-turn))))
         (tool-call (td-acp--field (td-acp--field request 'params) 'toolCall))
         (summary (format "%s -> %s"
                          (or (td-acp--field tool-call 'title) "Permission")
                          outcome)))
    (setf (td-acp-turn-permissions turn)
          (append (td-acp-turn-permissions turn)
                  (list (list :summary summary :raw raw-request))))
    (setf (td-acp-turn-updated-at turn) (td-acp--now))
    (td-acp--set-session-updated session)))

(defun td-acp--record-terminal-event (session summary raw)
  "Append terminal event SUMMARY and RAW data to SESSION."
  (let ((turn (or (td-acp--current-turn session t)
                  (td-acp--append-turn session (td-acp--make-turn)))))
    (setf (td-acp-turn-terminal-events turn)
          (append (td-acp-turn-terminal-events turn)
                  (list (list :summary summary :raw raw))))
    (setf (td-acp-turn-updated-at turn) (td-acp--now))
    (td-acp--set-session-updated session)))

(defun td-acp--record-unknown-update (session update)
  "Store raw session UPDATE on SESSION."
  (let ((turn (or (td-acp--current-turn session t)
                  (td-acp--append-turn session (td-acp--make-turn)))))
    (setf (td-acp-turn-unknown-updates turn)
          (append (td-acp-turn-unknown-updates turn) (list update)))
    (setf (td-acp-turn-updated-at turn) (td-acp--now))
    (td-acp--set-session-updated session)))

(defun td-acp--apply-session-update (session update)
  "Apply ACP session UPDATE to SESSION."
  (let* ((kind (td-acp--field update 'sessionUpdate))
         (turn (or (td-acp--current-turn session)
                   (td-acp--append-turn session (td-acp--make-turn)))))
    (pcase kind
      ("user_message_chunk"
       (unless (td-acp-session-pending-user-echo session)
         (setf (td-acp-turn-user turn)
               (td-acp--append-text (td-acp-turn-user turn)
                                    (td-acp--content->text update)))))
      ("agent_message_chunk"
       (setf (td-acp-turn-assistant turn)
             (td-acp--append-text (td-acp-turn-assistant turn)
                                  (td-acp--content->text update)))
       (setf (td-acp-session-status session) "streaming")
       (setf (td-acp-session-pending-user-echo session) nil))
      ("agent_thought_chunk"
       (setf (td-acp-turn-thoughts turn)
             (td-acp--append-text (td-acp-turn-thoughts turn)
                                  (td-acp--content->text update)))
       (setf (td-acp-session-pending-user-echo session) nil))
      ("tool_call"
       (setf (td-acp-turn-tool-calls turn)
             (append (td-acp-turn-tool-calls turn) (list update)))
       (setf (td-acp-session-pending-user-echo session) nil))
      ("tool_call_update"
       (let* ((tool-call-id (or (td-acp--field update 'toolCallId)
                                (td-acp--field update 'tool_call_id)))
              (existing (td-acp--find-tool-call turn tool-call-id)))
         (if existing
             (td-acp--merge-tool-call existing update)
           (setf (td-acp-turn-tool-calls turn)
                 (append (td-acp-turn-tool-calls turn) (list update)))))
       (setf (td-acp-session-pending-user-echo session) nil))
      ("plan"
       (td-acp--record-unknown-update session update))
      ("available_commands_update"
       (setf (td-acp-session-available-commands session)
             (td-acp--field update 'availableCommands)))
      ("current_mode_update"
       (setf (td-acp-session-current-mode-id session)
             (td-acp--field update 'currentModeId)))
      ("config_option_update"
       (setf (td-acp-session-config-options session)
             (td-acp--field update 'configOptions)))
      ("session_info_update"
       (when-let ((title (td-acp--field update 'title)))
         (setf (td-acp-session-title session) title))
       (when-let ((updated-at (td-acp--field update 'updatedAt)))
         (setf (td-acp-session-updated-at session) updated-at)))
      ("usage_update"
       (setf (td-acp-session-usage session) update))
      (_
       (td-acp--record-unknown-update session update)))
    (setf (td-acp-turn-updated-at turn) (td-acp--now))
    (td-acp--set-session-updated session)
    (td-acp--persist-session session)))

(defun td-acp--session-from-file (file)
  "Read local session metadata from Org FILE."
  (with-temp-buffer
    (insert-file-contents file nil 0 4096)
    (let ((title nil)
          (session-id nil)
          (created-at nil)
          (updated-at nil)
          (agent-command nil)
          (load-session nil))
      (goto-char (point-min))
      (while (re-search-forward
              "^#\\+\\(TITLE\\|PROPERTY: TD_ACP_\\([A-Z_]+\\)\\)[: ]+\\(.*\\)$"
              nil t)
        (let ((kind (match-string 1))
              (property (match-string 2))
              (value (string-trim (match-string 3))))
          (cond
           ((string= kind "TITLE")
            (setq title value))
           ((string= property "SESSION_ID")
            (setq session-id value))
           ((string= property "CREATED_AT")
            (setq created-at value))
           ((string= property "UPDATED_AT")
            (setq updated-at value))
           ((string= property "AGENT_COMMAND")
            (setq agent-command value))
           ((string= property "LOAD_SESSION")
            (setq load-session (string= value "true"))))))
      (list :file file
            :session-id session-id
            :title title
            :created-at created-at
            :updated-at updated-at
            :agent-command agent-command
            :load-session load-session))))

(defun td-acp--session-files (project-root)
  "Return transcript files for PROJECT-ROOT."
  (let ((dir (td-acp--sessions-dir project-root)))
    (if (file-directory-p dir)
        (directory-files dir t "\\.org\\'")
      nil)))

(defun td-acp--session-candidates (project-root)
  "Return metadata for PROJECT-ROOT local sessions."
  (mapcar #'td-acp--session-from-file
          (td-acp--session-files project-root)))

(defun td-acp--refresh-session-list-buffers ()
  "Refresh all visible session list buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'td-acp-session-list-mode)
        (revert-buffer)))))

(defun td-acp--metadata-at-point ()
  "Return the session metadata for the row at point."
  (when-let ((session-id (tabulated-list-get-id)))
    (alist-get session-id td-acp--session-list-metadata nil nil #'equal)))

(defun td-acp--read-session-metadata (project-root)
  "Prompt for a local session in PROJECT-ROOT and return its metadata."
  (let* ((metadata (td-acp--session-candidates project-root))
         (choices
          (mapcar (lambda (item)
                    (cons (format "%s  [%s]"
                                  (or (plist-get item :title) (plist-get item :session-id))
                                  (or (plist-get item :updated-at) ""))
                          item))
                  metadata)))
    (unless choices
      (user-error "No local ACP sessions"))
    (cdr (assoc (completing-read "Session: " choices nil t) choices))))

(defun td-acp--normalize-path (path)
  "Return PATH as an expanded absolute path."
  (directory-file-name (expand-file-name path)))

(defun td-acp--in-project-p (session path)
  "Return non-nil when PATH is inside SESSION project root."
  (file-in-directory-p (td-acp--normalize-path path)
                       (td-acp--normalize-path (td-acp-session-project-root session))))

(defun td-acp--allow-option (options)
  "Return preferred allow option from OPTIONS.

Prefer one-shot allow options over persistent allow options."
  (or (cl-find-if
       (lambda (option)
         (member (or (td-acp--field option 'kind) "")
                 '("allow_once" "allow")))
       options)
      (cl-find-if
       (lambda (option)
         (string-prefix-p "allow" (or (td-acp--field option 'kind) "")))
       options)))

(defun td-acp--reject-option (options)
  "Return first reject option from OPTIONS."
  (cl-find-if
   (lambda (option)
     (string-prefix-p "reject" (or (td-acp--field option 'kind) "")))
   options))

(defun td-acp--permission-options (params)
  "Return normalized permission options from PARAMS."
  (let ((options (td-acp--field params 'options)))
    (cond
     ((vectorp options) (append options nil))
     ((listp options) options)
     (t nil))))

(defun td-acp--permission-option-id (option)
  "Return stable option id for OPTION."
  (cond
   ((stringp option) option)
   (t
    (or (td-acp--field option 'optionId)
        (td-acp--field option 'id)
        (td-acp--field option 'name)
        (td-acp--field option 'kind)))))

(defun td-acp--permission-option-label (option)
  "Return display label for permission OPTION."
  (let* ((name (or (td-acp--field option 'name)
                   (td-acp--field option 'title)
                   (td-acp--permission-option-id option)
                   "Option"))
         (kind (td-acp--field option 'kind))
         (description (or (td-acp--field option 'description)
                          (td-acp--field option 'message)
                          (td-acp--field option 'summary))))
    (string-join
     (delq nil
           (list name
                 (when (and kind (not (string-empty-p (string-trim kind))))
                   (format "[%s]" kind))
                 (when (and description (not (string-empty-p (string-trim description))))
                   description)))
     " - ")))

(defun td-acp--permission-request-prompt (request)
  "Return minibuffer prompt for permission REQUEST."
  (let* ((params (td-acp--field request 'params))
         (tool-call (td-acp--field params 'toolCall))
         (message (or (td-acp--field params 'message)
                      (td-acp--field params 'title)))
         (subject (or (td-acp--tool-call-heading tool-call)
                      (td-acp--field tool-call 'title)
                      (td-acp--field tool-call 'kind)
                      "request")))
    (format "%s for %s: "
            (or message "ACP permission")
            subject)))

(defun td-acp--permission-request-risky-p (request session)
  "Return non-nil if REQUEST should be confirmed for SESSION."
  (let* ((params (td-acp--field request 'params))
         (tool-call (td-acp--field params 'toolCall))
         (raw-input (td-acp--field tool-call 'rawInput))
         (kind (or (td-acp--field tool-call 'kind) ""))
         (locations (td-acp--field tool-call 'locations))
         (command (or (td-acp--field raw-input 'command)
                      (td-acp--field raw-input 'cmd)))
         (path (or (td-acp--field raw-input 'path)
                   (td-acp--field raw-input 'file_path))))
    (or (member kind '("execute" "terminal" "edit" "write"))
        (and command (not (string-empty-p (string-trim command))))
        (and path
             (not (td-acp--in-project-p session path)))
        (cl-some (lambda (location)
                   (let ((path (td-acp--field location 'path)))
                     (and path (not (td-acp--in-project-p session path)))))
                 locations))))

(defun td-acp--select-permission-option (request session)
  "Resolve permission REQUEST for SESSION."
  (let* ((params (td-acp--field request 'params))
         (options (td-acp--permission-options params))
         (policy td-acp-permission-policy))
    (cond
     ((functionp policy)
      (funcall policy request session))
     ((eq policy 'allow-all)
      (or (td-acp--permission-option-id
           (or (td-acp--allow-option options) (car options)))
          'cancel))
     ((eq policy 'deny-all)
      (or (td-acp--permission-option-id
           (or (td-acp--reject-option options) (car options)))
          'cancel))
     ((and (eq policy 'ask-risky)
           (not (td-acp--permission-request-risky-p request session)))
      (or (td-acp--permission-option-id
           (or (td-acp--allow-option options) (car options)))
          'cancel))
     (t
      (let* ((choices
              (mapcar (lambda (option)
                        (cons (td-acp--permission-option-label option)
                              option))
                      options))
             (selection (cdr (assoc (completing-read
                                     (td-acp--permission-request-prompt request)
                                     choices nil t)
                                    choices))))
        (or (td-acp--permission-option-id selection) 'cancel))))))

(defun td-acp--make-error (message &optional data)
  "Return an ACP-compatible error object with MESSAGE and DATA."
  (if (fboundp 'acp-make-error)
      (acp-make-error :code -32000 :message message :data data)
    `((code . -32000) (message . ,message) (data . ,data))))

(defun td-acp--response (request-id result)
  "Build generic response object for REQUEST-ID and RESULT."
  `((:request-id . ,request-id)
    (:result . ,result)))

(defun td-acp--permission-response (request-id choice)
  "Build a correct ACP permission response for REQUEST-ID and CHOICE."
  (if (eq choice 'cancel)
      `((:request-id . ,request-id)
        (:result . ((outcome . "cancelled"))))
    `((:request-id . ,request-id)
      (:result . ((outcome . "selected")
                  (optionId . ,choice))))))

(defun td-acp--terminal-exit-status (terminal)
  "Return ACP exit status object for TERMINAL."
  `((exitCode . ,(td-acp-terminal-exit-code terminal))
    (signal . ,(td-acp-terminal-signal terminal))))

(defun td-acp--terminal-trim-output (output byte-limit)
  "Trim OUTPUT to BYTE-LIMIT bytes from the beginning."
  (if (or (null byte-limit) (<= (string-bytes output) byte-limit))
      output
    (let ((trimmed output))
      (while (> (string-bytes trimmed) byte-limit)
        (setq trimmed (substring trimmed 1)))
      trimmed)))

(defun td-acp--terminal-create (session params request-id)
  "Handle terminal/create PARAMS for SESSION with REQUEST-ID."
  (let* ((terminal-id (format "term_%d" (cl-incf td-acp--terminal-counter)))
         (command (td-acp--field params 'command))
         (args (append (or (td-acp--field params 'args) []) nil))
         (cwd (or (td-acp--field params 'cwd) (td-acp-session-project-root session)))
         (env (append (or (td-acp--field params 'env) []) nil))
         (output-byte-limit (td-acp--field params 'outputByteLimit))
         (process-environment
          (append
           (mapcar (lambda (entry)
                     (format "%s=%s"
                             (td-acp--field entry 'name)
                             (or (td-acp--field entry 'value) "")))
                   env)
           process-environment))
         (terminal (make-td-acp-terminal :id terminal-id
                                         :session-id (td-acp-session-id session)
                                         :command command
                                         :args args
                                         :cwd cwd
                                         :output ""
                                         :output-byte-limit output-byte-limit
                                         :waiters nil)))
    (let ((default-directory cwd))
      (setf (td-acp-terminal-process terminal)
            (make-process
             :name terminal-id
             :command (cons command args)
             :connection-type 'pipe
             :coding 'utf-8
             :noquery t
             :buffer (generate-new-buffer (format " *%s*" terminal-id))
             :file-handler t
             :stderr (generate-new-buffer (format " *%s-stderr*" terminal-id))
             :filter
             (lambda (_process chunk)
               (setf (td-acp-terminal-output terminal)
                     (td-acp--terminal-trim-output
                      (concat (td-acp-terminal-output terminal) chunk)
                      (td-acp-terminal-output-byte-limit terminal))))
             :sentinel
             (lambda (process _event)
               (unless (process-live-p process)
                 (setf (td-acp-terminal-exit-code terminal) (process-exit-status process))
                 (dolist (waiter (td-acp-terminal-waiters terminal))
                   (td-acp--acp-send-response
                    session
                    (td-acp--response waiter (td-acp--terminal-exit-status terminal))))
                 (setf (td-acp-terminal-waiters terminal) nil))))))
    (puthash terminal-id terminal td-acp--terminals)
    (td-acp--record-terminal-event
     session
     (format "Created %s %s" command (string-join args " "))
     params)
    (td-acp--persist-session session)
    (td-acp--acp-send-response session
                               (td-acp--response request-id `((terminalId . ,terminal-id))))))

(defun td-acp--terminal-output (session params request-id)
  "Handle terminal/output PARAMS for SESSION with REQUEST-ID."
  (let* ((terminal-id (td-acp--field params 'terminalId))
         (terminal (gethash terminal-id td-acp--terminals)))
    (if (not terminal)
        (td-acp--acp-send-response
         session
         `((:request-id . ,request-id)
           (:error . ,(td-acp--make-error "Unknown terminal" `((terminalId . ,terminal-id))))))
      (td-acp--record-terminal-event session (format "Output requested for %s" terminal-id) params)
      (td-acp--persist-session session)
      (td-acp--acp-send-response
       session
       (td-acp--response
        request-id
        `((output . ,(td-acp-terminal-output terminal))
          (truncated . ,(and (td-acp-terminal-output-byte-limit terminal)
                             (> (string-bytes (td-acp-terminal-output terminal))
                                (td-acp-terminal-output-byte-limit terminal))))
          (exitStatus . ,(and (not (process-live-p (td-acp-terminal-process terminal)))
                              (td-acp--terminal-exit-status terminal)))))))))

(defun td-acp--terminal-wait-for-exit (session params request-id)
  "Handle terminal/wait_for_exit PARAMS for SESSION with REQUEST-ID."
  (let* ((terminal-id (td-acp--field params 'terminalId))
         (terminal (gethash terminal-id td-acp--terminals)))
    (if (not terminal)
        (td-acp--acp-send-response
         session
         `((:request-id . ,request-id)
           (:error . ,(td-acp--make-error "Unknown terminal" `((terminalId . ,terminal-id))))))
      (if (process-live-p (td-acp-terminal-process terminal))
          (setf (td-acp-terminal-waiters terminal)
                (append (td-acp-terminal-waiters terminal) (list request-id)))
        (td-acp--acp-send-response
         session
         (td-acp--response request-id (td-acp--terminal-exit-status terminal)))))))

(defun td-acp--terminal-kill (session params request-id)
  "Handle terminal/kill PARAMS for SESSION with REQUEST-ID."
  (let* ((terminal-id (td-acp--field params 'terminalId))
         (terminal (gethash terminal-id td-acp--terminals)))
    (when (and terminal (process-live-p (td-acp-terminal-process terminal)))
      (ignore-errors (kill-process (td-acp-terminal-process terminal))))
    (td-acp--record-terminal-event session (format "Killed %s" terminal-id) params)
    (td-acp--persist-session session)
    (td-acp--acp-send-response session (td-acp--response request-id nil))))

(defun td-acp--terminal-release (session params request-id)
  "Handle terminal/release PARAMS for SESSION with REQUEST-ID."
  (let* ((terminal-id (td-acp--field params 'terminalId))
         (terminal (gethash terminal-id td-acp--terminals)))
    (when terminal
      (when (process-live-p (td-acp-terminal-process terminal))
        (ignore-errors (kill-process (td-acp-terminal-process terminal))))
      (setf (td-acp-terminal-released-p terminal) t))
    (td-acp--record-terminal-event session (format "Released %s" terminal-id) params)
    (td-acp--persist-session session)
    (td-acp--acp-send-response session (td-acp--response request-id nil))))

(defun td-acp--read-text-file (path line limit)
  "Return file contents for PATH, starting at LINE with LIMIT lines."
  (let* ((visited (find-buffer-visiting path))
         (text (if (buffer-live-p visited)
                   (with-current-buffer visited
                     (buffer-substring-no-properties (point-min) (point-max)))
                 (with-temp-buffer
                   (insert-file-contents path)
                   (buffer-string))))
         (lines (split-string text "\n")))
    (setq line (max 1 (or line 1)))
    (let* ((start (1- line))
           (slice (nthcdr (min start (length lines)) lines)))
      (when limit
        (setq slice (cl-subseq slice 0 (min limit (length slice)))))
      (string-join slice "\n"))))

(defun td-acp--write-text-file (path content)
  "Write CONTENT to PATH, preferring a visiting buffer when present."
  (make-directory (file-name-directory path) t)
  (if-let ((buffer (find-buffer-visiting path)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert content)
          (write-region content nil path nil 'silent)
          (set-buffer-modified-p nil)
          (set-visited-file-modtime)))
    (with-temp-file path
      (insert content))))


;; ACP adapter

(defun td-acp--acp-make-client (session)
  "Create a new upstream ACP client for SESSION."
  (td-acp--assert-acp)
  (acp-make-client :context-buffer (td-acp-session-prompt-buffer session)
                   :command td-acp-agent-program
                   :command-params td-acp-agent-args))

(defun td-acp--acp-send-request (session request on-success on-failure)
  "Send ACP REQUEST for SESSION."
  (acp-send-request :client (td-acp-session-client session)
                    :request request
                    :buffer (td-acp-session-prompt-buffer session)
                    :on-success on-success
                    :on-failure on-failure))

(defun td-acp--acp-send-notification (session notification)
  "Send ACP NOTIFICATION for SESSION."
  (acp-send-notification :client (td-acp-session-client session)
                         :notification notification))

(defun td-acp--acp-send-response (session response)
  "Send ACP RESPONSE for SESSION."
  (acp-send-response :client (td-acp-session-client session)
                     :response response))

(defun td-acp--acp-initialize-request ()
  "Build initialize request including terminal capability."
  `((:method . "initialize")
    (:params . ((protocolVersion . 1)
                (clientInfo . ,td-acp-client-info)
                (clientCapabilities . ((fs . ((readTextFile . t)
                                              (writeTextFile . t)))
                                       (terminal . t)))))))

(defun td-acp--acp-prompt-request (session prompt)
  "Build session/prompt request for SESSION with PROMPT."
  `((:method . "session/prompt")
    (:params . ((sessionId . ,(td-acp-session-id session))
                (prompt . [((type . "text")
                            (text . ,prompt))])))))

(defun td-acp--acp-new-request (session)
  "Build session/new request for SESSION."
  (if (fboundp 'acp-make-session-new-request)
      (acp-make-session-new-request :cwd (td-acp-session-project-root session))
    `((:method . "session/new")
      (:params . ((cwd . ,(td-acp-session-project-root session))
                  (mcpServers . []))))))

(defun td-acp--acp-authenticate-request ()
  "Build optional authenticate request."
  (when td-acp-auth-method-id
    (if (fboundp 'acp-make-authenticate-request)
        (acp-make-authenticate-request :method-id td-acp-auth-method-id
                                       :method td-acp-auth-method)
      `((:method . "authenticate")
        (:params . ((methodId . ,td-acp-auth-method-id)
                    ,@(when td-acp-auth-method
                        `((authMethod . ,td-acp-auth-method)))))))))

(defun td-acp--acp-load-request (session)
  "Build session/load request for SESSION."
  (if (fboundp 'acp-make-session-load-request)
      (acp-make-session-load-request :session-id (td-acp-session-id session)
                                     :cwd (td-acp-session-project-root session))
    `((:method . "session/load")
      (:params . ((sessionId . ,(td-acp-session-id session))
                  (cwd . ,(td-acp-session-project-root session))
                  (mcpServers . []))))))

(defun td-acp--acp-cancel-notification (session)
  "Build session/cancel notification for SESSION."
  (if (fboundp 'acp-make-session-cancel-notification)
      (acp-make-session-cancel-notification :session-id (td-acp-session-id session)
                                            :reason "Cancelled from Emacs")
    `((:method . "session/cancel")
      (:params . ((sessionId . ,(td-acp-session-id session))
                  (reason . "Cancelled from Emacs"))))))


;; ACP runtime

(defun td-acp--handle-notification (session notification)
  "Handle incoming ACP NOTIFICATION for SESSION."
  (pcase (td-acp--field notification 'method)
    ("session/update"
     (let* ((params (td-acp--field notification 'params))
            (session-id (td-acp--field params 'sessionId))
            (update (td-acp--field params 'update))
            (target (or (and session-id (td-acp--lookup-session session-id))
                        session)))
       (when target
         (td-acp--apply-session-update target update))))
    (_ nil)))

(defun td-acp--handle-request (session request)
  "Handle incoming ACP REQUEST for SESSION."
  (let* ((method (td-acp--field request 'method))
         (request-id (td-acp--field request 'id))
         (params (td-acp--field request 'params)))
    (cond
     ((equal method "session/request_permission")
      (cl-pushnew request-id (td-acp-session-pending-permission-ids session) :test #'equal)
     (let ((choice (td-acp--select-permission-option request session)))
        (setf (td-acp-session-pending-permission-ids session)
              (delete request-id (td-acp-session-pending-permission-ids session)))
        (td-acp--record-permission-event
         session request request (if (eq choice 'cancel) "cancelled" choice))
        (td-acp--persist-session session)
        (td-acp--acp-send-response
         session
         (td-acp--permission-response request-id choice))))
     ((equal method "fs/read_text_file")
      (let ((path (td-acp--field params 'path)))
        (condition-case err
            (td-acp--acp-send-response
             session
             (if (fboundp 'acp-make-fs-read-text-file-response)
                 (acp-make-fs-read-text-file-response
                  :request-id request-id
                  :content (td-acp--read-text-file path
                                                   (td-acp--field params 'line)
                                                   (td-acp--field params 'limit)))
               (td-acp--response
                request-id
                `((content . ,(td-acp--read-text-file path
                                                      (td-acp--field params 'line)
                                                      (td-acp--field params 'limit)))))))
          (error
           (td-acp--acp-send-response
            session
            (if (fboundp 'acp-make-fs-read-text-file-response)
                (acp-make-fs-read-text-file-response
                 :request-id request-id
                 :error (td-acp--make-error (error-message-string err)))
              `((:request-id . ,request-id)
                (:error . ,(td-acp--make-error (error-message-string err))))))))))
     ((equal method "fs/write_text_file")
      (condition-case err
          (progn
            (td-acp--write-text-file (td-acp--field params 'path)
                                     (td-acp--field params 'content))
            (td-acp--acp-send-response
             session
             (if (fboundp 'acp-make-fs-write-text-file-response)
                 (acp-make-fs-write-text-file-response :request-id request-id)
               (td-acp--response request-id nil))))
        (error
         (td-acp--acp-send-response
          session
          (if (fboundp 'acp-make-fs-write-text-file-response)
              (acp-make-fs-write-text-file-response
               :request-id request-id
               :error (td-acp--make-error (error-message-string err)))
            `((:request-id . ,request-id)
              (:error . ,(td-acp--make-error (error-message-string err)))))))))
     ((equal method "terminal/create")
      (td-acp--terminal-create session params request-id))
     ((equal method "terminal/output")
      (td-acp--terminal-output session params request-id))
     ((equal method "terminal/wait_for_exit")
      (td-acp--terminal-wait-for-exit session params request-id))
     ((equal method "terminal/kill")
      (td-acp--terminal-kill session params request-id))
     ((equal method "terminal/release")
      (td-acp--terminal-release session params request-id))
     (t
      (td-acp--acp-send-response
       session
       `((:request-id . ,request-id)
         (:error . ,(td-acp--make-error (format "Unsupported method %s" method)))))))))

(defun td-acp--handle-error (session error)
  "Record ACP process ERROR for SESSION."
  (setf (td-acp-session-last-error session) error)
  (setf (td-acp-session-status session) "error")
  (td-acp--set-session-updated session)
  (td-acp--persist-session session)
  (force-mode-line-update t))

(defun td-acp--shutdown-session (session)
  "Shut down SESSION client if live."
  (when-let ((client (td-acp-session-client session)))
    (when (fboundp 'acp-shutdown)
      (ignore-errors (acp-shutdown :client client))))
  (setf (td-acp-session-client session) nil
        (td-acp-session-live-p session) nil))

(defun td-acp--session-bootstrap-success (session result)
  "Handle successful initialize RESULT for SESSION."
  (let ((agent-capabilities (td-acp--field result 'agentCapabilities)))
    (setf (td-acp-session-load-session-capable-p session)
          (and agent-capabilities
               (td-acp--field agent-capabilities 'loadSession))))
  (let ((continue
         (lambda ()
           (if (td-acp-session-id session)
               (if (td-acp-session-load-session-capable-p session)
                   (td-acp--acp-send-request
                    session
                    (td-acp--acp-load-request session)
                    (lambda (load-result)
                      (setf (td-acp-session-config-options session) (td-acp--field load-result 'configOptions))
                      (when-let ((modes (td-acp--field load-result 'modes)))
                        (setf (td-acp-session-current-mode-id session)
                              (td-acp--field modes 'currentModeId)))
                      (setf (td-acp-session-status session) "connected"
                            (td-acp-session-live-p session) t)
                      (td-acp--set-session-updated session)
                      (td-acp--display-session session))
                    (lambda (error)
                      (setf (td-acp-session-live-p session) nil
                            (td-acp-session-status session) "history-only"
                            (td-acp-session-last-error session) error)
                      (td-acp--display-session session)))
                 (setf (td-acp-session-live-p session) nil
                       (td-acp-session-status session) "history-only")
                 (td-acp--display-session session))
             (td-acp--acp-send-request
              session
              (td-acp--acp-new-request session)
              (lambda (new-result)
                (setf (td-acp-session-id session) (td-acp--field new-result 'sessionId))
                (setf (td-acp-session-title session)
                      (or (td-acp-session-title session)
                          (td-acp-session-id session)))
                (setf (td-acp-session-live-p session) t
                      (td-acp-session-status session) "idle")
                (when-let ((config-options (td-acp--field new-result 'configOptions)))
                  (setf (td-acp-session-config-options session) config-options))
                (when-let ((modes (td-acp--field new-result 'modes)))
                  (setf (td-acp-session-current-mode-id session)
                        (td-acp--field modes 'currentModeId)))
                (td-acp--put-session session)
                (td-acp--ensure-transcript-file session)
                (td-acp--rename-prompt-buffer session)
                (td-acp--persist-session session)
                (td-acp--display-session session))
              (lambda (error)
                (setf (td-acp-session-status session) "error"
                      (td-acp-session-last-error session) error)
                (td-acp--persist-session session)
                (message "ACP session/new failed: %s" error)))))))
    (if-let ((auth-request (td-acp--acp-authenticate-request)))
        (td-acp--acp-send-request
         session
         auth-request
         (lambda (_response)
           (funcall continue))
         (lambda (error)
           (setf (td-acp-session-status session) "error"
                 (td-acp-session-last-error session) error)
           (td-acp--persist-session session)
           (message "ACP authenticate failed: %s" error)))
      (funcall continue))))

(defun td-acp--connect-session (session)
  "Create and initialize upstream ACP client for SESSION."
  (td-acp--shutdown-session session)
  (setf (td-acp-session-status session) "connecting"
        (td-acp-session-agent-command session) (td-acp--command-string)
        (td-acp-session-client session) (td-acp--acp-make-client session))
  (acp-subscribe-to-notifications
   :client (td-acp-session-client session)
   :buffer (td-acp-session-prompt-buffer session)
   :on-notification (lambda (notification)
                      (td-acp--handle-notification session notification)))
  (acp-subscribe-to-requests
   :client (td-acp-session-client session)
   :buffer (td-acp-session-prompt-buffer session)
   :on-request (lambda (request)
                 (td-acp--handle-request session request)))
  (acp-subscribe-to-errors
   :client (td-acp-session-client session)
   :buffer (td-acp-session-prompt-buffer session)
   :on-error (lambda (error)
               (td-acp--handle-error session error)))
  (td-acp--acp-send-request
   session
   (td-acp--acp-initialize-request)
   (lambda (result)
     (td-acp--session-bootstrap-success session result))
   (lambda (error)
     (setf (td-acp-session-status session) "error"
           (td-acp-session-last-error session) error)
     (td-acp--persist-session session)
     (message "ACP initialize failed: %s" error))))


;; Interactive commands

(defun td-acp--make-session (project-root &optional metadata)
  "Instantiate a session object for PROJECT-ROOT and optional METADATA."
  (let ((session
         (make-td-acp-session
          :id (plist-get metadata :session-id)
          :title (or (plist-get metadata :title) "ACP Session")
          :project-root project-root
          :transcript-file (plist-get metadata :file)
          :agent-command (or (plist-get metadata :agent-command) (td-acp--command-string))
          :created-at (or (plist-get metadata :created-at) (td-acp--now))
          :updated-at (or (plist-get metadata :updated-at) (td-acp--now))
          :status (if (plist-get metadata :session-id) "history-only" "connecting")
          :turns nil
          :load-session-capable-p (plist-get metadata :load-session)
          :pending-permission-ids nil
          :live-p nil)))
    (td-acp--register-session session)
    (td-acp--ensure-prompt-buffer session)
    session))

;;;###autoload
(defun td-acp-session-new ()
  "Create a new ACP session for the current project."
  (interactive)
  (let* ((project-root (td-acp--project-root))
         (session (td-acp--make-session project-root)))
    (td-acp--display-session session)
    (td-acp--connect-session session)))

(defun td-acp--open-session-from-metadata (project-root metadata)
  "Open local session in PROJECT-ROOT described by METADATA."
  (let* ((session-id (plist-get metadata :session-id))
         (existing (and session-id (td-acp--lookup-session session-id)))
         (session (or existing (td-acp--make-session project-root metadata))))
    (if existing
        (td-acp--display-session existing)
      (if (plist-get metadata :load-session)
          (progn
            (setf (td-acp-session-status session) "connecting")
            (td-acp--connect-session session))
        (setf (td-acp-session-status session) "history-only")
        (td-acp--display-session session)))
    session))

;;;###autoload
(defun td-acp-session-open (&optional metadata)
  "Open an existing ACP session for the current project.

When METADATA is nil, prompt from local transcripts."
  (interactive)
  (let* ((project-root (td-acp--project-root))
         (metadata (or metadata (td-acp--read-session-metadata project-root))))
    (td-acp--open-session-from-metadata project-root metadata)))

;;;###autoload
(defun td-acp-session-open-transcript ()
  "Open transcript for the current or selected ACP session."
  (interactive)
  (let ((session (or td-acp--session
                     (let* ((project-root (td-acp--project-root))
                            (metadata (td-acp--read-session-metadata project-root)))
                       (td-acp--open-session-from-metadata project-root metadata)))))
    (pop-to-buffer (td-acp--open-transcript-buffer session))))

;;;###autoload
(defun td-acp-session-send ()
  "Send prompt from the current ACP prompt buffer."
  (interactive)
  (unless (and td-acp--session (td-acp-session-id td-acp--session))
    (user-error "This buffer is not attached to a live ACP session"))
  (unless (td-acp-session-client td-acp--session)
    (user-error "This session is not connected"))
  (let ((prompt (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (when (string-empty-p prompt)
      (user-error "Prompt is empty"))
    (let ((turn (td-acp--make-turn prompt)))
      (td-acp--append-turn td-acp--session turn)
      (setf (td-acp-session-status td-acp--session) "sending"
            (td-acp-session-pending-user-echo td-acp--session) prompt)
      (td-acp--persist-session td-acp--session)
      (erase-buffer)
      (td-acp--acp-send-request
       td-acp--session
       (td-acp--acp-prompt-request td-acp--session prompt)
       (lambda (result)
         (setf (td-acp-turn-prompt-response (td-acp--current-turn td-acp--session t)) result)
         (setf (td-acp-session-status td-acp--session) "idle"
               (td-acp-session-pending-user-echo td-acp--session) nil)
         (td-acp--persist-session td-acp--session))
       (lambda (error)
         (setf (td-acp-turn-prompt-response (td-acp--current-turn td-acp--session t))
               `((error . ,error)))
         (setf (td-acp-session-status td-acp--session) "error"
               (td-acp-session-pending-user-echo td-acp--session) nil
               (td-acp-session-last-error td-acp--session) error)
         (td-acp--persist-session td-acp--session))))))

;;;###autoload
(defun td-acp-session-cancel ()
  "Cancel the active prompt turn for the current ACP session."
  (interactive)
  (unless td-acp--session
    (user-error "Not in an ACP prompt buffer"))
  (unless (and (td-acp-session-id td-acp--session)
               (td-acp-session-client td-acp--session))
    (user-error "Session is not connected"))
  (setf (td-acp-session-status td-acp--session) "cancelling")
  (td-acp--persist-session td-acp--session)
  (dolist (request-id (td-acp-session-pending-permission-ids td-acp--session))
    (td-acp--acp-send-response
     td-acp--session
     (if (fboundp 'acp-make-session-request-permission-response)
         (acp-make-session-request-permission-response
          :request-id request-id
          :cancelled t)
       `((:request-id . ,request-id)
         (:result . ((outcome . ((outcome . "cancelled")))))))))
  (setf (td-acp-session-pending-permission-ids td-acp--session) nil)
  (td-acp--acp-send-notification td-acp--session
                                 (td-acp--acp-cancel-notification td-acp--session)))

;;;###autoload
(defun td-acp-session-reconnect ()
  "Reconnect the current ACP session."
  (interactive)
  (unless td-acp--session
    (user-error "Not in an ACP prompt buffer"))
  (unless (td-acp-session-id td-acp--session)
    (user-error "Session does not have an id yet"))
  (td-acp--connect-session td-acp--session))

(defun td-acp--recent-transcript-context (session)
  "Return a compact transcript excerpt for SESSION."
  (let* ((turns (last (td-acp-session-turns session) 2))
         (chunks
          (mapcar (lambda (turn)
                    (format "User:\n%s\n\nAssistant:\n%s"
                            (or (td-acp-turn-user turn) "")
                            (or (td-acp-turn-assistant turn) "")))
                  turns)))
    (string-trim (string-join chunks "\n\n"))))

;;;###autoload
(defun td-acp-session-fork ()
  "Create a new session prefilled with recent context from the current one."
  (interactive)
  (unless td-acp--session
    (user-error "Not in an ACP prompt buffer"))
  (let* ((project-root (td-acp-session-project-root td-acp--session))
         (context (td-acp--recent-transcript-context td-acp--session))
         (new-session (td-acp--make-session project-root)))
    (td-acp--display-session new-session)
    (with-current-buffer (td-acp--ensure-prompt-buffer new-session)
      (insert (string-trim
               (format "Continue this work from prior session %s.\n\n%s"
                       (td-acp-session-id td-acp--session)
                       context))))
    (td-acp--connect-session new-session)))


;; Session list mode

(defvar td-acp-session-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'td-acp-session-list-open)
    (define-key map (kbd "n") #'td-acp-session-new)
    (define-key map (kbd "g") #'revert-buffer)
    (define-key map (kbd "p") #'td-acp-session-list-open-prompt)
    (define-key map (kbd "t") #'td-acp-session-list-open-transcript)
    map)
  "Keymap for `td-acp-session-list-mode'.")

(define-derived-mode td-acp-session-list-mode tabulated-list-mode "ACP Sessions"
  "Major mode for browsing project ACP sessions."
  (setq tabulated-list-format [("Title" 32 t)
                               ("Updated" 24 t)
                               ("Status" 16 t)
                               ("Session" 30 t)])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'td-acp--session-list-refresh nil t)
  (tabulated-list-init-header))

(defun td-acp--session-list-refresh ()
  "Refresh current session list buffer."
  (setq td-acp--session-list-metadata nil)
  (setq tabulated-list-entries
        (mapcar
         (lambda (metadata)
           (let* ((session-id (plist-get metadata :session-id))
                  (live (and session-id (td-acp--lookup-session session-id)))
                  (status (cond
                           (live (td-acp-session-status live))
                           ((plist-get metadata :load-session) "saved")
                           (t "history-only"))))
             (push (cons session-id metadata) td-acp--session-list-metadata)
             (list session-id
                   (vector (or (plist-get metadata :title) session-id "")
                           (or (plist-get metadata :updated-at) "")
                           status
                           (or session-id "")))))
         (sort (td-acp--session-candidates td-acp--session-list-root)
               (lambda (left right)
                 (string> (or (plist-get left :updated-at) "")
                          (or (plist-get right :updated-at) ""))))))
  (tabulated-list-print t))

;;;###autoload
(defun td-acp-session-list ()
  "List local ACP sessions for the current project."
  (interactive)
  (let* ((project-root (td-acp--project-root))
         (buffer (get-buffer-create
                  (format "*td-acp-sessions:%s*"
                          (td-acp--project-name project-root)))))
    (with-current-buffer buffer
      (td-acp-session-list-mode)
      (setq-local td-acp--session-list-root project-root)
      (td-acp--session-list-refresh))
    (pop-to-buffer buffer)))

(defun td-acp-session-list-open ()
  "Open session at point from `td-acp-session-list-mode'."
  (interactive)
  (let ((metadata (td-acp--metadata-at-point)))
    (unless metadata
      (user-error "No session on this row"))
    (td-acp--open-session-from-metadata td-acp--session-list-root metadata)))

(defun td-acp-session-list-open-prompt ()
  "Open session prompt at point."
  (interactive)
  (let ((session (td-acp-session-list-open)))
    (pop-to-buffer (td-acp-session-prompt-buffer session))))

(defun td-acp-session-list-open-transcript ()
  "Open transcript for session at point."
  (interactive)
  (let ((session (td-acp-session-list-open)))
    (pop-to-buffer (td-acp--open-transcript-buffer session))))


;; Prompt mode

(defvar td-acp-prompt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'td-acp-session-send)
    (define-key map (kbd "C-c C-k") #'td-acp-session-cancel)
    (define-key map (kbd "C-c C-o") #'td-acp-session-open-transcript)
    (define-key map (kbd "C-c C-r") #'td-acp-session-reconnect)
    (define-key map (kbd "C-c C-f") #'td-acp-session-fork)
    map)
  "Keymap for `td-acp-prompt-mode'.")

(define-derived-mode td-acp-prompt-mode text-mode "ACP Prompt"
  "Major mode for composing prompts for an ACP session."
  (setq-local require-final-newline nil)
  (setq-local mode-line-process nil)
  (visual-line-mode 1))

(provide 'td-acp)

;;; td-acp.el ends here
