;;; vterm-agent.el --- Tmux-backed agent sessions for Vterm -*- lexical-binding: t; coding: utf-8; -*-

;; This is a local integration layer for durable CLI agent sessions.

;;; Commentary:

;; `vterm-agent' keeps session management in Emacs and uses Vterm as
;; the terminal surface.  Sessions are tmux sessions addressed as:
;;
;;   local:<tmux-session>
;;   ssh:<host>:<tmux-session>
;;
;; Code-cells buffers send prompts into the tmux session, capture pane
;; output back into the cells buffer, and use OSC 9 / OSC 777
;; notifications from agent CLIs to update run status when available.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)

(declare-function vterm-mode "vterm" ())
(declare-function code-cells--bounds "code-cells" (&optional count use-region no-header))
(declare-function code-cells-mode "code-cells" (&optional arg))
(declare-function posframe-hide "posframe" (buffer-or-name))
(declare-function posframe-show "posframe" (buffer-or-name &rest args))
(declare-function posframe-workable-p "posframe" ())

(defvar vterm-shell)
(defvar vterm--process)

(defgroup vterm-agent nil
  "Durable CLI agent sessions backed by tmux and Vterm."
  :group 'terminals
  :prefix "vterm-agent-")

(defcustom vterm-agent-known-ssh-hosts nil
  "SSH hosts offered by Vterm Agent session prompts.
An empty host means local tmux."
  :type '(repeat string))

(defcustom vterm-agent-agent-commands
  '(("codex" . "codex --no-alt-screen")
    ("claude" . "claude"))
  "Named agent commands offered by `vterm-agent-new-session'."
  :type '(alist :key-type string :value-type string))

(defcustom vterm-agent-notification-function
  #'vterm-agent-message-notification
  "Function called with a normalized OSC notification event plist.
Set to nil to disable Emacs-side notification messages."
  :type '(choice (const :tag "Disabled" nil) function))

(defcustom vterm-agent-posframe-timeout 8
  "Seconds before a Vterm Agent posframe notification disappears."
  :type 'number)

(defcustom vterm-agent-posframe-width 44
  "Text width used for Vterm Agent posframe notifications."
  :type 'integer)

(defcustom vterm-agent-posframe-max-body-lines 3
  "Maximum body lines shown in a Vterm Agent posframe notification."
  :type 'integer)

(defcustom vterm-agent-tmux-buffer-name "vterm-agent"
  "tmux buffer name used when pasting prompts into agent sessions."
  :type 'string)

(defcustom vterm-agent-submit-keys '("Enter")
  "tmux key names sent after pasting a prompt.
The default submits Codex prompts after bracketed paste.  Customize
this if an agent uses a different submit key sequence."
  :type '(repeat string))

(defcustom vterm-agent-result-comment-prefix "#"
  "Prefix used for status lines in Vterm Agent cells buffers."
  :type 'string)

(defcustom vterm-agent-keep-shell-on-exit t
  "Keep a shell open when a newly spawned agent command exits.
This makes startup failures visible in the tmux pane instead of
letting the detached session disappear immediately."
  :type 'boolean)

(defcustom vterm-agent-capture-output-after-send t
  "When non-nil, poll tmux pane output after sending a cell.
This is the fallback path for agents that do not emit terminal
notifications with full response text."
  :type 'boolean)

(defcustom vterm-agent-capture-poll-interval 1.0
  "Seconds between tmux pane output captures for an active cell run."
  :type 'number)

(defcustom vterm-agent-capture-timeout 600
  "Maximum seconds to keep polling tmux output for a cell run."
  :type 'number)

(defcustom vterm-agent-capture-max-lines 160
  "Maximum number of tmux pane lines rendered into a cell result."
  :type 'integer)

(defvar vterm-agent-event-hook nil
  "Hook run with one argument, a normalized Vterm Agent event plist.")

(defvar vterm-agent-posframe-buffer " *vterm-agent-notification*"
  "Buffer name used for Vterm Agent posframe notifications.")

(defvar vterm-agent--posframe-timer nil
  "Timer used to hide the current Vterm Agent posframe notification.")

(defface vterm-agent-posframe
  '((t :inherit tooltip))
  "Base face for Vterm Agent posframe notifications."
  :group 'vterm-agent)

(defface vterm-agent-posframe-title
  '((t :inherit font-lock-keyword-face :weight bold))
  "Title face for Vterm Agent posframe notifications."
  :group 'vterm-agent)

(defface vterm-agent-posframe-meta
  '((t :inherit shadow))
  "Metadata face for Vterm Agent posframe notifications."
  :group 'vterm-agent)

(defface vterm-agent-posframe-icon
  '((t :weight bold :height 1.2))
  "Status icon face for Vterm Agent posframe notifications."
  :group 'vterm-agent)

(defface vterm-agent-posframe-complete
  '((t :inherit success :weight bold))
  "Completion status face for Vterm Agent posframe notifications."
  :group 'vterm-agent)

(defface vterm-agent-posframe-request
  '((t :inherit warning :weight bold))
  "Request status face for Vterm Agent posframe notifications."
  :group 'vterm-agent)

(defface vterm-agent-posframe-error
  '((t :inherit error :weight bold))
  "Error status face for Vterm Agent posframe notifications."
  :group 'vterm-agent)

(cl-defstruct vterm-agent--session
  id host name status attached-buffer cells-buffer latest-event events)

(defvar vterm-agent--sessions (make-hash-table :test 'equal)
  "Known Vterm Agent sessions keyed by session id.")

(defvar-local vterm-agent-session-id nil
  "Vterm Agent session id associated with the current buffer.")

(defvar-local vterm-agent--osc-carry ""
  "Incomplete OSC bytes carried across Vterm process-filter chunks.")

(defvar-local vterm-agent--runs nil
  "Hash table of cell run ids to run plists in a cells buffer.")

(defvar-local vterm-agent--last-run-id 0
  "Last local run id allocated in a cells buffer.")

(defvar-local vterm-agent--list-host nil
  "Host currently shown by `vterm-agent-sessions-mode'.")

(defconst vterm-agent--tmux-list-format
  "#{session_name}\t#{session_created}\t#{session_attached}"
  "tmux list-sessions format used by Vterm Agent.")

(defconst vterm-agent--eval-and-new-cell-keys
  '("C-c C-<return>"
    "C-c C-RET"
    "C-c <C-return>"
    "C-c <return>"
    "C-c RET"
    "C-c C-m"
    "C-c C-j"
    "C-c C-n")
  "Key sequences that send the current cell and create a new one.")

(defvar vterm-agent-cells-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'vterm-agent-send-cell)
    (define-key map (kbd "C-c C-r") #'vterm-agent-refresh-output)
    (dolist (key vterm-agent--eval-and-new-cell-keys)
      (define-key map (kbd key) #'vterm-agent-send-cell-and-new-cell))
    map)
  "Keymap for `vterm-agent-cells-mode'.")

(defvar vterm-agent-sessions-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'vterm-agent-sessions-attach)
    (define-key map (kbd "c") #'vterm-agent-sessions-cells)
    (define-key map (kbd "k") #'vterm-agent-sessions-kill)
    (define-key map (kbd "g") #'revert-buffer)
    map)
  "Keymap for `vterm-agent-sessions-mode'.")

(defun vterm-agent--session-id-for (host name)
  "Return a Vterm Agent session id for HOST and tmux session NAME."
  (if (and host (not (string-empty-p host)))
      (format "ssh:%s:%s" host name)
    (format "local:%s" name)))

(defun vterm-agent--parse-session-id (id)
  "Parse session ID into a plist with `:host' and `:name'."
  (cond
   ((string-match "\\`local:\\(.+\\)\\'" id)
    (list :host nil :name (match-string 1 id)))
   ((string-match "\\`ssh:\\([^:]+\\):\\(.+\\)\\'" id)
    (list :host (match-string 1 id) :name (match-string 2 id)))
   (t
    (user-error "Invalid Vterm Agent session id: %s" id))))

(defun vterm-agent--remember-session (host name &optional status)
  "Record and return a session for HOST and tmux session NAME."
  (let* ((id (vterm-agent--session-id-for host name))
         (session (or (gethash id vterm-agent--sessions)
                      (make-vterm-agent--session
                       :id id :host host :name name :events nil))))
    (when status
      (setf (vterm-agent--session-status session) status))
    (puthash id session vterm-agent--sessions)
    session))

(defun vterm-agent--shell-command (argv)
  "Return a shell command string for ARGV."
  (mapconcat #'shell-quote-argument argv " "))

(defun vterm-agent--call (program args &optional input)
  "Run PROGRAM with ARGS, optionally feeding INPUT on stdin.
Return stdout as a string.  Signal an error when the process exits
with a non-zero status."
  (let ((out (generate-new-buffer " *vterm-agent-out*")))
    (unwind-protect
        (let ((status
               (if input
                   (with-temp-buffer
                     (insert input)
                     (apply #'call-process-region
                            (point-min) (point-max)
                            program nil (list out t) nil args))
                 (with-current-buffer out
                   (apply #'process-file program nil (list out t) nil args)))))
          (unless (eq status 0)
            (error "%s failed: %s"
                   program
                   (with-current-buffer out (string-trim (buffer-string)))))
          (with-current-buffer out
            (buffer-string)))
      (when (buffer-live-p out) (kill-buffer out)))))

(defun vterm-agent--tmux (host args &optional input)
  "Run tmux ARGS locally or on HOST, optionally feeding INPUT."
  (if (and host (not (string-empty-p host)))
      (vterm-agent--call
       "ssh" (list host (vterm-agent--shell-command (cons "tmux" args))) input)
    (vterm-agent--call "tmux" args input)))

(defun vterm-agent--list-sessions (host)
  "Return tmux sessions on HOST as Vterm Agent session structs."
  (condition-case err
      (let ((output (vterm-agent--tmux
                     host (list "list-sessions" "-F"
                                vterm-agent--tmux-list-format))))
        (cl-loop for line in (split-string output "\n" t)
                 for fields = (split-string line "\t")
                 for name = (nth 0 fields)
                 for attached = (string-to-number (or (nth 2 fields) "0"))
                 for status = (if (> attached 0) "attached" "detached")
                 when (and name (not (string-empty-p name)))
                 collect (let ((session (vterm-agent--remember-session
                                         host name)))
                           (unless (vterm-agent--session-latest-event session)
                             (setf (vterm-agent--session-status session)
                                   status))
                           session)))
    (error
     (let ((message (error-message-string err)))
       (if (string-match-p
            (regexp-opt '("no server running" "failed to connect to server"
                          "no current client"))
            message)
           nil
         (signal (car err) (cdr err)))))))

(defun vterm-agent--read-host (&optional prompt)
  "Read an SSH host, returning nil for local."
  (let ((input (completing-read
                (or prompt "SSH host (blank for local): ")
                vterm-agent-known-ssh-hosts nil nil nil nil "")))
    (unless (string-empty-p input) input)))

(defun vterm-agent--candidate-sessions (host)
  "Return completing-read candidates for HOST."
  (mapcar (lambda (session)
            (cons (vterm-agent--session-id session) session))
          (vterm-agent--list-sessions host)))

(defun vterm-agent--current-session ()
  "Return the current buffer's associated session, if any."
  (and vterm-agent-session-id
       (gethash vterm-agent-session-id vterm-agent--sessions)))

(defun vterm-agent--read-session (&optional prompt-host)
  "Read and return a Vterm Agent session.
When PROMPT-HOST is non-nil, prompt for a host before listing tmux sessions."
  (let* ((current (and (not prompt-host) (vterm-agent--current-session)))
         (host (if current
                   (vterm-agent--session-host current)
                 (vterm-agent--read-host)))
         (candidates (vterm-agent--candidate-sessions host)))
    (unless candidates
      (user-error "No tmux sessions found for %s" (or host "local host")))
    (cdr (assoc (completing-read
                 "Agent session: " candidates nil t
                 nil nil (or (and current
                                  (vterm-agent--session-id current))
                             (caar candidates)))
                candidates))))

(defun vterm-agent--default-session-name (agent)
  "Return a conservative default tmux session name for AGENT."
  (let* ((root (when (fboundp 'project-current)
                 (when-let* ((project (project-current nil)))
                   (when (fboundp 'project-root)
                     (project-root project)))))
         (base (file-name-nondirectory
                (directory-file-name (or root default-directory))))
         (name (downcase (format "%s-%s" agent base))))
    (setq name (replace-regexp-in-string "[^[:alnum:]_.-]+" "-" name))
    (setq name (replace-regexp-in-string "\\`-+\\|-+\\'" "" name))
    (when (string-empty-p name)
      (setq name "agent"))
    (when (string-prefix-p "-" name)
      (setq name (concat "agent-" name)))
    name))

(defun vterm-agent--remote-directory-default (host)
  "Return a default remote directory string for HOST."
  (if (and host (file-remote-p default-directory))
      (or (file-remote-p default-directory 'localname) "~")
    (expand-file-name default-directory)))

(defun vterm-agent--read-agent-command ()
  "Read an agent command and return (LABEL . COMMAND)."
  (let* ((labels (append (mapcar #'car vterm-agent-agent-commands)
                         '("custom")))
         (label (completing-read "Agent: " labels nil t nil nil "codex"))
         (command (if (string= label "custom")
                      (read-shell-command "Command: ")
                    (or (cdr (assoc label vterm-agent-agent-commands))
                        label))))
    (cons label command)))

(defun vterm-agent--wrapped-command (command)
  "Return shell wrapper for COMMAND.
The wrapper leaves an interactive shell open when COMMAND exits so
startup errors remain visible after `tmux new-session -d'."
  (if (not vterm-agent-keep-shell-on-exit)
      command
    (concat
     "printf '%s\\n' "
     (shell-quote-argument (format "[vterm-agent] starting: %s" command))
     "; "
     command
     "; __vterm_agent_status=$?; "
     "printf '\\n[vterm-agent] command exited with status %s. "
     "Leaving shell open.\\n' \"$__vterm_agent_status\"; "
     "exec \"${SHELL:-/bin/sh}\"")))

;;;###autoload
(defun vterm-agent-new-session (host name directory command)
  "Create a detached tmux session running COMMAND in DIRECTORY.
When HOST is nil, create a local tmux session.  Otherwise create it
through SSH on HOST."
  (interactive
   (let* ((host (vterm-agent--read-host))
          (agent (vterm-agent--read-agent-command))
          (default-name (vterm-agent--default-session-name (car agent)))
          (name (read-string "tmux session: " default-name))
          (directory (if host
                         (read-string "Remote directory: "
                                      (vterm-agent--remote-directory-default host))
                       (read-directory-name "Directory: " default-directory nil t)))
          (command (read-shell-command "Command: " (cdr agent))))
     (list host name directory command)))
  (when (string-empty-p name)
    (user-error "Session name cannot be empty"))
  (vterm-agent--tmux host
                       (append
                        (list "new-session" "-d" "-s" name "-c" directory)
                        (unless host
                          (list "-e" (concat "PATH=" (getenv "PATH"))))
                        (list (vterm-agent--wrapped-command command))))
  (let ((session (vterm-agent--remember-session host name "detached")))
    (message "Created %s" (vterm-agent--session-id session))
    session))

(defun vterm-agent--attach-command (session)
  "Return the shell command used to attach Vterm to SESSION."
  (let ((host (vterm-agent--session-host session))
        (name (vterm-agent--session-name session)))
    (if host
        (vterm-agent--shell-command
         (list "ssh" "-tt" host "tmux" "attach" "-t" name))
      (vterm-agent--shell-command
       (list "tmux" "attach" "-t" name)))))

(defun vterm-agent--live-vterm-buffer-p (buffer)
  "Return non-nil when BUFFER has a live Vterm process."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (boundp 'vterm--process)
              vterm--process
              (process-live-p vterm--process)))))

;;;###autoload
(defun vterm-agent-attach (session)
  "Open Vterm attached to SESSION."
  (interactive (list (vterm-agent--read-session current-prefix-arg)))
  (unless (require 'vterm nil t)
    (user-error "vterm is not available"))
  (let* ((id (vterm-agent--session-id session))
         (buffer-name (format "*vterm-agent: %s*" id))
         (buffer (get-buffer buffer-name)))
    (if (and buffer (vterm-agent--live-vterm-buffer-p buffer))
        (pop-to-buffer buffer)
      (when buffer
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer)))
      (setq buffer (get-buffer-create buffer-name))
      (pop-to-buffer buffer)
      (with-current-buffer buffer
        (let ((vterm-shell (vterm-agent--attach-command session)))
          (vterm-mode))
        (setq-local vterm-agent-session-id id)
        (setq-local vterm-agent--osc-carry "")))
    (setf (vterm-agent--session-attached-buffer session) buffer
          (vterm-agent--session-status session) "attached")
    (puthash id session vterm-agent--sessions)
    buffer))

(defun vterm-agent--kill-session (session)
  "Kill tmux SESSION and remove it from the live session table."
  (let ((host (vterm-agent--session-host session))
        (name (vterm-agent--session-name session))
        (id (vterm-agent--session-id session)))
    (vterm-agent--tmux host (list "kill-session" "-t" name))
    (remhash id vterm-agent--sessions)
    (vterm-agent--refresh-session-buffers)
    id))

;;;###autoload
(defun vterm-agent-kill-session (session)
  "Kill tmux-backed Vterm Agent SESSION."
  (interactive (list (vterm-agent--read-session current-prefix-arg)))
  (let ((id (vterm-agent--session-id session)))
    (unless (yes-or-no-p (format "Kill Vterm Agent session %s? " id))
      (user-error "Canceled"))
    (vterm-agent--kill-session session)
    (message "Killed %s" id)
    id))

(defun vterm-agent--selected-session-from-list ()
  "Return the session at point in a session list buffer."
  (let ((id (tabulated-list-get-id)))
    (unless id
      (user-error "No session on this line"))
    (or (gethash id vterm-agent--sessions)
        (let* ((parsed (vterm-agent--parse-session-id id))
               (host (plist-get parsed :host))
               (name (plist-get parsed :name)))
          (vterm-agent--remember-session host name)))))

(defun vterm-agent-sessions-attach ()
  "Attach to the session at point."
  (interactive)
  (vterm-agent-attach (vterm-agent--selected-session-from-list)))

(defun vterm-agent-sessions-cells ()
  "Open cells for the session at point."
  (interactive)
  (vterm-agent-cells (vterm-agent--selected-session-from-list)))

(defun vterm-agent-sessions-kill ()
  "Kill the session at point."
  (interactive)
  (vterm-agent-kill-session (vterm-agent--selected-session-from-list)))

(defun vterm-agent--latest-event-summary (session)
  "Return a short status string for SESSION's latest event."
  (when-let* ((event (vterm-agent--session-latest-event session)))
    (let ((title (plist-get event :title))
          (body (plist-get event :body)))
      (string-trim
       (string-join (delq nil (list title body)) ": ")))))

(defun vterm-agent--tabulated-entries ()
  "Return `tabulated-list-entries' for the current session list."
  (mapcar
   (lambda (session)
     (let* ((id (vterm-agent--session-id session))
            (host (or (vterm-agent--session-host session) "local"))
            (status (or (vterm-agent--session-status session) "unknown"))
            (latest (or (vterm-agent--latest-event-summary session) "")))
       (list id (vector (vterm-agent--session-name session)
                        host status latest))))
   (vterm-agent--list-sessions vterm-agent--list-host)))

(define-derived-mode vterm-agent-sessions-mode tabulated-list-mode
  "Vterm-Agent-Sessions"
  "Major mode for Vterm Agent tmux session lists."
  (setq tabulated-list-format
        [("Session" 28 t)
         ("Where" 18 t)
         ("Status" 12 t)
         ("Latest" 48 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Session" nil))
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm)
                (setq tabulated-list-entries
                      (vterm-agent--tabulated-entries))
                (tabulated-list-print t)))
  (tabulated-list-init-header))

;;;###autoload
(defun vterm-agent-list-sessions (host)
  "List local or remote tmux sessions in a buffer.
HOST is nil for local tmux."
  (interactive (list (vterm-agent--read-host)))
  (let ((buffer (get-buffer-create "*vterm-agent-sessions*")))
    (with-current-buffer buffer
      (vterm-agent-sessions-mode)
      (setq-local vterm-agent--list-host host)
      (revert-buffer))
    (pop-to-buffer buffer)))

(defun vterm-agent--ensure-runs ()
  "Ensure the current cells buffer has a run table."
  (unless (hash-table-p vterm-agent--runs)
    (setq-local vterm-agent--runs (make-hash-table :test 'equal))))

(defun vterm-agent--new-run-id ()
  "Return a new local cells run id."
  (vterm-agent--ensure-runs)
  (number-to-string (cl-incf vterm-agent--last-run-id)))

(defun vterm-agent--remove-run-result (run-id)
  "Remove rendered result text for RUN-ID in the current buffer."
  (let ((pos (point-min))
        next)
    (while (< pos (point-max))
      (setq next (next-single-property-change
                  pos 'vterm-agent-run-id nil (point-max)))
      (if (equal (get-text-property pos 'vterm-agent-run-id) run-id)
          (delete-region pos next)
        (setq pos next)))))

(defun vterm-agent--run-result-range (run-id)
  "Return the rendered result range for RUN-ID, or nil."
  (let ((pos (point-min))
        next)
    (catch 'found
      (while (< pos (point-max))
        (setq next (next-single-property-change
                    pos 'vterm-agent-run-id nil (point-max)))
        (when (equal (get-text-property pos 'vterm-agent-run-id) run-id)
          (throw 'found (cons pos next)))
        (setq pos next))
      nil)))

(defun vterm-agent--run-result-end (run-id)
  "Return the end of RUN-ID's rendered result, or nil."
  (cdr (vterm-agent--run-result-range run-id)))

(defun vterm-agent--run-status-line (run-id run)
  "Return the rendered status text for RUN-ID and RUN."
  (let* ((status (plist-get run :status))
         (message (or (plist-get run :message) ""))
         (output (or (plist-get run :output) ""))
         (prefix vterm-agent-result-comment-prefix)
         (first (format "\n%s => vterm-agent run %s [%s]\n"
                        prefix run-id status))
         (body (unless (string-empty-p message)
                 (mapconcat (lambda (line)
                              (format "%s    %s" prefix line))
                            (split-string message "\n")
                            "\n")))
         (captured (unless (string-empty-p output)
                     (concat
                      (format "%s --- output ---\n" prefix)
                      (mapconcat
                       (lambda (line)
                         (format "%s    %s" prefix line))
                       (split-string output "\n")
                       "\n")))))
    (concat first
            body
            (and body "\n")
            captured
            (and captured "\n"))))

(defun vterm-agent--render-run (run-id)
  "Render RUN-ID status in the current cells buffer."
  (vterm-agent--ensure-runs)
  (when-let* ((run (gethash run-id vterm-agent--runs))
              (marker (plist-get run :result-marker)))
    (let ((inhibit-read-only t)
          (text (vterm-agent--run-status-line run-id run)))
      (set-marker-insertion-type marker nil)
      (save-excursion
        (vterm-agent--remove-run-result run-id)
        (goto-char marker)
        (insert (propertize text
                            'vterm-agent-result t
                            'vterm-agent-run-id run-id
                            'font-lock-face 'font-lock-comment-face
                            'rear-nonsticky t))))))

(defun vterm-agent--buffer-substring-without-results (start end)
  "Return text from START to END excluding Vterm Agent result blocks."
  (let ((pos start)
        (chunks nil))
    (while (< pos end)
      (let ((next (next-single-property-change
                   pos 'vterm-agent-result nil end)))
        (unless (get-text-property pos 'vterm-agent-result)
          (push (buffer-substring-no-properties pos next) chunks))
        (setq pos next)))
    (apply #'concat (nreverse chunks))))

(defun vterm-agent--cell-bounds ()
  "Return the current cell bounds as (START END)."
  (cond
   ((use-region-p)
    (list (region-beginning) (region-end)))
   ((fboundp 'code-cells--bounds)
    (pcase-let ((`(,start ,end) (code-cells--bounds 1 t t)))
      (list start end)))
   (t
    (save-excursion
      (let ((end (progn (forward-paragraph) (point)))
            (start (progn (backward-paragraph) (point))))
        (list start end))))))

(defun vterm-agent--send-to-session (session text)
  "Paste TEXT into tmux SESSION and send `vterm-agent-submit-keys'."
  (let ((host (vterm-agent--session-host session))
        (name (vterm-agent--session-name session)))
    (vterm-agent--tmux host
                         (list "load-buffer" "-b"
                               vterm-agent-tmux-buffer-name "-")
                         text)
    (vterm-agent--tmux host
                         (list "paste-buffer" "-p" "-r" "-b"
                               vterm-agent-tmux-buffer-name "-t" name))
    (when vterm-agent-submit-keys
      (vterm-agent--tmux host
                           (append (list "send-keys" "-t" name)
                                   vterm-agent-submit-keys)))))

(defun vterm-agent--strip-terminal-escapes (text)
  "Return TEXT with common terminal escape sequences removed."
  (let ((clean text))
    (setq clean
          (replace-regexp-in-string "\e\\][^\a\e]*\\(\a\\|\e\\\\\\)"
                                    "" clean t t))
    (setq clean
          (replace-regexp-in-string "\e\\[[0-?]*[ -/]*[@-~]"
                                    "" clean t t))
    (setq clean
          (replace-regexp-in-string "\e." "" clean t t))
    clean))

(defun vterm-agent--normalize-captured-output (output)
  "Normalize tmux pane OUTPUT for rendering in a cells buffer."
  (let* ((clean (vterm-agent--strip-terminal-escapes output))
         (clean (replace-regexp-in-string "\r" "\n" clean t t))
         (lines (split-string clean "\n"))
         (trimmed-lines (mapcar #'string-trim-right lines)))
    (string-trim (string-join trimmed-lines "\n"))))

(defun vterm-agent--capture-session-output (session)
  "Return the latest tmux pane output for SESSION."
  (let* ((host (vterm-agent--session-host session))
         (name (vterm-agent--session-name session))
         (start (format "-%d" (max 1 vterm-agent-capture-max-lines))))
    (vterm-agent--normalize-captured-output
     (vterm-agent--tmux host
                          (list "capture-pane" "-p" "-J"
                                "-S" start "-t" name)))))

(defun vterm-agent--session-by-id (session-id)
  "Return a remembered session for SESSION-ID, creating it if needed."
  (or (gethash session-id vterm-agent--sessions)
      (let* ((parsed (vterm-agent--parse-session-id session-id)))
        (vterm-agent--remember-session
         (plist-get parsed :host)
         (plist-get parsed :name)))))

(defun vterm-agent--latest-run-id ()
  "Return the latest run id in the current cells buffer."
  (vterm-agent--ensure-runs)
  (let ((latest-id nil)
        (latest-num -1))
    (maphash
     (lambda (run-id _run)
       (let ((num (string-to-number run-id)))
         (when (> num latest-num)
           (setq latest-num num
                 latest-id run-id))))
     vterm-agent--runs)
    latest-id))

(defun vterm-agent--run-id-at-point ()
  "Return the rendered run id at point, if point is in a result block."
  (or (get-text-property (point) 'vterm-agent-run-id)
      (when-let* ((pos (previous-single-property-change
                       (point) 'vterm-agent-run-id nil (point-min))))
        (get-text-property (1- pos) 'vterm-agent-run-id))))

(defun vterm-agent--poll-run-output (buffer run-id)
  "Poll tmux output for RUN-ID in BUFFER and reschedule while active."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let* ((run (and (hash-table-p vterm-agent--runs)
                            (gethash run-id vterm-agent--runs)))
                  (session-id (plist-get run :session-id))
                  (session (vterm-agent--session-by-id session-id))
                  (started-at (plist-get run :capture-started-at)))
        (condition-case err
            (let* ((output (vterm-agent--capture-session-output session))
                   (old-output (or (plist-get run :output) ""))
                   (status (plist-get run :status))
                   (elapsed (- (float-time) started-at))
                   (terminal-status-p
                    (member status '("complete" "error" "request"))))
              (unless (string= output old-output)
                (setq run (plist-put run :output output))
                (puthash run-id run vterm-agent--runs)
                (vterm-agent--render-run run-id))
              (cond
               (terminal-status-p nil)
               ((>= elapsed vterm-agent-capture-timeout)
                (setq run (plist-put run :status "captured"))
                (setq run (plist-put
                           run :message
                           "Output capture timed out; refresh with C-c C-r"))
                (puthash run-id run vterm-agent--runs)
                (vterm-agent--render-run run-id))
               (t
                (run-at-time vterm-agent-capture-poll-interval nil
                             #'vterm-agent--poll-run-output buffer run-id))))
          (error
           (setq run (plist-put run :status "capture-error"))
           (setq run (plist-put run :message (error-message-string err)))
           (puthash run-id run vterm-agent--runs)
           (vterm-agent--render-run run-id)))))))

(defun vterm-agent--schedule-output-capture (run-id)
  "Schedule tmux output capture for RUN-ID in the current buffer."
  (when vterm-agent-capture-output-after-send
    (vterm-agent--ensure-runs)
    (when-let* ((run (gethash run-id vterm-agent--runs)))
      (setq run (plist-put run :capture-started-at (float-time)))
      (puthash run-id run vterm-agent--runs)
      (run-at-time vterm-agent-capture-poll-interval nil
                   #'vterm-agent--poll-run-output
                   (current-buffer) run-id))))

;;;###autoload
(defun vterm-agent-refresh-output (&optional run-id)
  "Refresh tmux pane output for RUN-ID or the latest run."
  (interactive)
  (unless vterm-agent-session-id
    (user-error "This buffer is not associated with a Vterm Agent session"))
  (let* ((run-id (or run-id
                     (vterm-agent--run-id-at-point)
                     (vterm-agent--latest-run-id)))
         (run (and run-id (gethash run-id vterm-agent--runs)))
         (session (vterm-agent--session-by-id vterm-agent-session-id)))
    (unless run
      (user-error "No Vterm Agent run found"))
    (setq run (plist-put run :output
                         (vterm-agent--capture-session-output session)))
    (when (member (plist-get run :status) '("sent" "running" "captured"))
      (setq run (plist-put run :status "captured"))
      (setq run (plist-put run :message "Output refreshed from tmux pane")))
    (puthash run-id run vterm-agent--runs)
    (vterm-agent--render-run run-id)
    run-id))

;;;###autoload
(defun vterm-agent-send-cell (start end)
  "Send the current code cell to the associated tmux-backed agent.
When called from `code-cells', START and END delimit the cell body."
  (interactive (vterm-agent--cell-bounds))
  (unless vterm-agent-session-id
    (user-error "This buffer is not associated with a Vterm Agent session"))
  (let* ((session (vterm-agent--session-by-id vterm-agent-session-id))
         (text (string-trim-right
                (vterm-agent--buffer-substring-without-results start end)))
         (run-id (vterm-agent--new-run-id))
         (marker (copy-marker end nil))
         (run (list :id run-id :session-id vterm-agent-session-id
                    :status "sent" :message "Prompt sent"
                    :result-marker marker :prompt text)))
    (when (string-empty-p text)
      (user-error "Cell is empty"))
    (puthash run-id run vterm-agent--runs)
    (vterm-agent--render-run run-id)
    (condition-case err
        (vterm-agent--send-to-session session text)
      (error
       (setq run (plist-put run :status "error"))
       (setq run (plist-put run :message (error-message-string err)))
       (puthash run-id run vterm-agent--runs)
       (vterm-agent--render-run run-id)
       (signal (car err) (cdr err))))
    (setq run (plist-put run :status "running"))
    (setq run (plist-put run :message "Prompt sent; capturing tmux output"))
    (puthash run-id run vterm-agent--runs)
    (vterm-agent--render-run run-id)
    (vterm-agent--schedule-output-capture run-id)
    run-id))

(defun vterm-agent--insert-new-cell-after-run (run-id)
  "Insert a fresh cell after RUN-ID's rendered result."
  (let ((inhibit-read-only t)
        (pos (or (vterm-agent--run-result-end run-id)
                 (point))))
    (goto-char pos)
    (unless (bolp)
      (insert "\n"))
    (let ((start (point)))
      (insert "\n# %%\n")
      (remove-text-properties
       start (point)
       '(vterm-agent-result nil
         vterm-agent-run-id nil
         font-lock-face nil)))))

;;;###autoload
(defun vterm-agent-send-cell-and-new-cell ()
  "Send the current cell and create a new empty cell below it."
  (interactive)
  (pcase-let* ((`(,start ,body-end) (vterm-agent--cell-bounds))
               (run-id (vterm-agent-send-cell start body-end)))
    (vterm-agent--insert-new-cell-after-run run-id)))

(define-derived-mode vterm-agent-cells-mode text-mode "Vterm-Agent-Cells"
  "Major mode for Vterm-backed agent code cells."
  (setq-local comment-start "#")
  (setq-local code-cells-boundary-regexp "^#\\s-*\\(%+\\)")
  (setq-local code-cells-eval-region-commands
              '((vterm-agent-cells-mode . vterm-agent-send-cell)))
  (require 'code-cells nil t)
  (when (fboundp 'code-cells-mode)
    (code-cells-mode 1))
  (vterm-agent--ensure-runs))

;;;###autoload
(defun vterm-agent-cells (session)
  "Open a code-cells buffer associated with SESSION."
  (interactive (list (vterm-agent--read-session current-prefix-arg)))
  (let* ((id (vterm-agent--session-id session))
         (buffer (get-buffer-create (format "*vterm-agent-cells: %s*" id))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'vterm-agent-cells-mode)
        (vterm-agent-cells-mode))
      (setq-local vterm-agent-session-id id)
      (vterm-agent--ensure-runs)
      (when (= (point-min) (point-max))
        (insert "# %%\n")))
    (setf (vterm-agent--session-cells-buffer session) buffer)
    (puthash id session vterm-agent--sessions)
    (pop-to-buffer buffer)))

(defun vterm-agent--event-kind (title body)
  "Classify an agent notification from TITLE and BODY."
  (let ((text (downcase (string-join (delq nil (list title body)) " "))))
    (cond
     ((string-match-p
       (regexp-opt '("error" "failed" "failure" "exception" "crashed"))
       text)
      'error)
     ((string-match-p
       (regexp-opt '("approval" "approve" "permission" "confirm" "input"
                     "blocked" "waiting" "needs your" "requires"))
       text)
      'request)
     ((string-match-p
       (regexp-opt '("complete" "completed" "done" "finished" "success"))
       text)
      'complete)
     (t 'notification))))

(defun vterm-agent--parse-osc-payload (payload raw session-id)
  "Parse OSC PAYLOAD into a normalized Vterm Agent event.
RAW is the full escape sequence.  SESSION-ID is the originating
Vterm Agent session id.  Return nil when PAYLOAD is not an OSC 9
or OSC 777 notification."
  (cond
   ((string-prefix-p "9;" payload)
    (let ((body (substring payload 2)))
      (unless (string-prefix-p "4;" body)
        (let ((kind (vterm-agent--event-kind nil body)))
          (list :session-id session-id
                :kind kind
                :title nil
                :body body
                :raw raw)))))
   ((string-prefix-p "777;" payload)
    (pcase-let* ((parts (split-string payload ";"))
                 (`(,_osc ,command ,title . ,body-parts) parts))
      (when (and command (string= (downcase command) "notify"))
        (let* ((body (string-join body-parts ";"))
               (title (and title (not (string-empty-p title)) title))
               (kind (vterm-agent--event-kind title body)))
          (list :session-id session-id
                :kind kind
                :title title
                :body body
                :raw raw)))))))

(defun vterm-agent--latest-pending-run-id ()
  "Return the latest pending run id in the current cells buffer."
  (vterm-agent--ensure-runs)
  (let ((latest-id nil)
        (latest-num -1))
    (maphash
     (lambda (run-id run)
       (when (and (equal (plist-get run :session-id) vterm-agent-session-id)
                  (member (plist-get run :status)
                          '("sent" "running" "captured" "notification")))
         (let ((num (string-to-number run-id)))
           (when (> num latest-num)
             (setq latest-num num
                   latest-id run-id)))))
     vterm-agent--runs)
    latest-id))

(defun vterm-agent--update-cells-for-event (event)
  "Update the associated cells buffer for EVENT."
  (let* ((session-id (plist-get event :session-id))
         (session (and session-id
                       (gethash session-id vterm-agent--sessions)))
         (buffer (and session
                      (vterm-agent--session-cells-buffer session))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when-let* ((run-id (vterm-agent--latest-pending-run-id))
                    (run (gethash run-id vterm-agent--runs)))
          (let* ((kind (plist-get event :kind))
                 (status (pcase kind
                           ('complete "complete")
                           ('request "request")
                           ('error "error")
                           (_ "notification")))
                 (message (string-trim
                           (string-join
                            (delq nil (list (plist-get event :title)
                                            (plist-get event :body)))
                            ": "))))
            (setq run (plist-put run :status status))
            (setq run (plist-put run :message message))
            (puthash run-id run vterm-agent--runs)
            (vterm-agent--render-run run-id)))))))

(defun vterm-agent--refresh-session-buffers ()
  "Refresh visible Vterm Agent session list buffers."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'vterm-agent-sessions-mode)
          (revert-buffer nil t))))))

(defun vterm-agent--status-face (kind)
  "Return the posframe status face for event KIND."
  (pcase kind
    ('complete 'vterm-agent-posframe-complete)
    ('request 'vterm-agent-posframe-request)
    ('error 'vterm-agent-posframe-error)
    (_ 'vterm-agent-posframe-meta)))

(defun vterm-agent--status-icon (kind)
  "Return a compact icon string for event KIND."
  (pcase kind
    ('complete "✓")
    ('request "?")
    ('error "!")
    (_ "✦")))

(defun vterm-agent--status-message (kind)
  "Return fallback body text for event KIND."
  (pcase kind
    ('complete "Task completed.")
    ('request "Input requested.")
    ('error "Task failed.")
    (_ "Agent notification.")))

(defun vterm-agent--wrap-text (text width max-lines)
  "Return TEXT wrapped to WIDTH and capped at MAX-LINES."
  (let* ((text (string-trim (or text "")))
         (text (replace-regexp-in-string "[ \t\n\r]+" " " text))
         (words (split-string text " " t))
         (lines nil)
         (line ""))
    (dolist (word words)
      (if (or (string-empty-p line)
              (<= (string-width (concat line " " word)) width))
          (setq line (if (string-empty-p line) word (concat line " " word)))
        (push line lines)
        (setq line word)))
    (unless (string-empty-p line)
      (push line lines))
    (setq lines (nreverse lines))
    (when (> (length lines) max-lines)
      (setq lines (seq-take lines max-lines))
      (setcar (last lines)
              (truncate-string-to-width
               (concat (car (last lines)) "...")
               width nil nil "...")))
    (mapconcat (lambda (line)
                 (truncate-string-to-width line width nil nil "..."))
               lines "\n")))

(defun vterm-agent--posframe-title (event)
  "Return the title shown for EVENT."
  (let ((title (plist-get event :title)))
    (cond
     ((and title (not (string-empty-p title))) title)
     ((plist-get event :session-id) "Vterm Agent")
     (t "Agent"))))

(defun vterm-agent--format-posframe-notification (event)
  "Return posframe notification text for EVENT."
  (let* ((kind (plist-get event :kind))
         (session-id (or (plist-get event :session-id) "unknown session"))
         (title (vterm-agent--posframe-title event))
         (body (or (plist-get event :body) ""))
         (width (max 24 vterm-agent-posframe-width))
         (text-width (max 20 (- width 2)))
         (icon (propertize
                (vterm-agent--status-icon kind)
                'face (list (vterm-agent--status-face kind)
                            'vterm-agent-posframe-icon)))
         (title (truncate-string-to-width title text-width nil nil "..."))
         (body (if (string-empty-p (string-trim body))
                   (vterm-agent--status-message kind)
                 body))
         (body (vterm-agent--wrap-text
                body text-width vterm-agent-posframe-max-body-lines))
         (body-lines (unless (string-empty-p body)
                       (split-string body "\n")))
         (session-line
          (propertize
           (truncate-string-to-width session-id text-width nil nil "...")
           'face 'vterm-agent-posframe-meta))
         (indent "  "))
    (string-join
     (append
      (list (concat icon " "
                    (propertize title 'face 'vterm-agent-posframe-title)))
      (mapcar (lambda (line) (concat indent line)) body-lines)
      (list (concat indent session-line)))
     "\n")))

(defun vterm-agent--posframe-top-right-poshandler (info)
  "Position a posframe like a top-right toast using posframe INFO."
  (let* ((parent-frame (plist-get info :parent-frame))
         (posframe-width (plist-get info :posframe-width))
         (margin 18)
         (top 36)
         (frame-width (frame-pixel-width parent-frame)))
    (cons (max margin (- frame-width posframe-width margin))
          top)))

(defun vterm-agent-hide-notification ()
  "Hide the current Vterm Agent posframe notification."
  (interactive)
  (when (timerp vterm-agent--posframe-timer)
    (cancel-timer vterm-agent--posframe-timer)
    (setq vterm-agent--posframe-timer nil))
  (when (require 'posframe nil t)
    (posframe-hide vterm-agent-posframe-buffer)))

(defun vterm-agent-posframe-notification (event)
  "Display EVENT as a top-right posframe notification.
Falls back to `vterm-agent-message-notification' when child frames
are unavailable."
  (if (and (display-graphic-p)
           (require 'posframe nil t)
           (or (not (fboundp 'posframe-workable-p))
               (posframe-workable-p)))
      (let* ((string (vterm-agent--format-posframe-notification event))
             (background (face-background 'vterm-agent-posframe nil t))
             (foreground (face-foreground 'vterm-agent-posframe nil t))
             (border (or (face-foreground 'vterm-agent-posframe-meta nil t)
                         foreground))
             (poshandler #'vterm-agent--posframe-top-right-poshandler))
        (vterm-agent-hide-notification)
        (posframe-show
         vterm-agent-posframe-buffer
         :string string
         :poshandler poshandler
         :background-color background
         :foreground-color foreground
         :internal-border-width 1
         :internal-border-color border
         :left-fringe 8
         :right-fringe 8
         :width vterm-agent-posframe-width
         :override-parameters
         '((undecorated . t)
           (skip-taskbar . t)
           (no-accept-focus . t)
           (no-focus-on-map . t)
           (min-width . 0)
           (min-height . 0)))
        (setq vterm-agent--posframe-timer
              (run-at-time vterm-agent-posframe-timeout nil
                           #'vterm-agent-hide-notification)))
    (vterm-agent-message-notification event)))

(defun vterm-agent-message-notification (event)
  "Display EVENT as an Emacs message."
  (let* ((session-id (plist-get event :session-id))
         (kind (plist-get event :kind))
         (title (plist-get event :title))
         (body (plist-get event :body))
         (summary (string-trim
                   (string-join (delq nil (list title body)) ": "))))
    (message "vterm-agent[%s] %s: %s" session-id kind summary)))

(defun vterm-agent--record-event (event)
  "Record and route a normalized EVENT."
  (let* ((session-id (plist-get event :session-id))
         (parsed (and session-id (vterm-agent--parse-session-id session-id)))
         (session (and parsed
                       (vterm-agent--remember-session
                        (plist-get parsed :host)
                        (plist-get parsed :name)))))
    (when session
      (setf (vterm-agent--session-latest-event session) event
            (vterm-agent--session-status session)
            (symbol-name (plist-get event :kind))
            (vterm-agent--session-events session)
            (cons event (vterm-agent--session-events session)))
      (puthash session-id session vterm-agent--sessions))
    (vterm-agent--update-cells-for-event event)
    (when vterm-agent-notification-function
      (funcall vterm-agent-notification-function event))
    (run-hook-with-args 'vterm-agent-event-hook event)
    (vterm-agent--refresh-session-buffers)))

(defun vterm-agent--scan-output (output)
  "Scan raw Vterm process OUTPUT for OSC notifications."
  (let ((text (concat vterm-agent--osc-carry output))
        (start 0)
        event)
    (setq vterm-agent--osc-carry "")
    (while (string-match "\e\\]\\([^\a\e]*\\)\\(\a\\|\e\\\\\\)" text start)
      (let* ((raw (match-string 0 text))
             (payload (match-string 1 text)))
        (setq start (match-end 0))
        (when (setq event
                    (vterm-agent--parse-osc-payload
                     payload raw vterm-agent-session-id))
          (vterm-agent--record-event event))))
    (let ((tail (substring text start)))
      (when (string-match "\e\\]" tail)
        (setq vterm-agent--osc-carry (substring tail (match-beginning 0)))
        (when (> (length vterm-agent--osc-carry) 4096)
          (setq vterm-agent--osc-carry
                (substring vterm-agent--osc-carry -4096)))))))

(defun vterm-agent--filter-advice (orig-fn process output)
  "Advice around Vterm ORIG-FN process filter for PROCESS OUTPUT."
  (when (and (process-live-p process)
             (buffer-live-p (process-buffer process)))
    (with-current-buffer (process-buffer process)
      (when vterm-agent-session-id
        (vterm-agent--scan-output output))))
  (funcall orig-fn process output))

(defun vterm-agent--install-filter-advice ()
  "Install Vterm process-filter advice."
  (when (fboundp 'vterm--filter)
    (unless (advice-member-p #'vterm-agent--filter-advice 'vterm--filter)
      (advice-add 'vterm--filter :around #'vterm-agent--filter-advice))))

(with-eval-after-load 'vterm
  (vterm-agent--install-filter-advice))

(provide 'vterm-agent)

;;; vterm-agent.el ends here
