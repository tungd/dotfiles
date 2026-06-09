;;; scv.el --- Emacs integration for scv sessions -*- lexical-binding: t; -*-

;; Local development package for scv Session Manager, Session Viewer, and
;; Rich Prompt Box integration.
;;
;; Package-Requires: ((emacs "27.1") (json "2.0") (cl-lib "1.0") (subr-x "1.0") (transient "0.3.0"))

;;; Commentary:
;; Session Manager, Rich Prompt Box, Session Viewer dispatch, and command
;; menu for the scv coding-session backend.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'tabulated-list)
(require 'transient nil t)

(declare-function scv-session-viewer-open "scv-session-viewer" (session-id))

(defconst scv--source-directory
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name default-directory))))
  "Directory containing the scv source checkout.")

(defgroup scv nil
  "Emacs surfaces for scv coding sessions."
  :group 'tools)

(defcustom scv-source-directory scv--source-directory
  "Source directory for the local scv checkout."
  :type 'directory
  :group 'scv)

(defcustom scv-executable nil
  "Executable path for scv.

When nil, prefer the built binary under `scv-source-directory', then
`scv' from PATH."
  :type '(choice (const :tag "Auto" nil)
                 file)
  :group 'scv)

(defcustom scv-default-origin "local"
  "Default Session Origin name for local scv sessions."
  :type 'string
  :group 'scv)

(defcustom scv-session-id-prefix "tc-"
  "Prefix for generated scv Session ids."
  :type 'string
  :group 'scv)

(defcustom scv-session-list-command '("session" "list" "--json")
  "scv arguments used to list sessions for `scv-session-manager'."
  :type '(repeat string)
  :group 'scv)

(defcustom scv-session-action-submit-command
  '("session" "action" "submit")
  "scv arguments used to submit prompt text from a Rich Prompt Box.

`scv-prompt-submit' appends --session-id, --kind, and then sends the
prompt text to stdin."
  :type '(repeat string)
  :group 'scv)

(defcustom scv-session-action-set-model-command
  '("session" "action" "set-model")
  "scv arguments used to set the active model for a Coding Session."
  :type '(repeat string)
  :group 'scv)

(defcustom scv-session-action-set-reasoning-command
  '("session" "action" "set-reasoning")
  "scv arguments used to set the active reasoning level."
  :type '(repeat string)
  :group 'scv)

(defcustom scv-session-action-set-permission-command
  '("session" "action" "set-permission")
  "scv arguments used to set the active permission mode."
  :type '(repeat string)
  :group 'scv)

(defcustom scv-session-action-set-skills-command
  '("session" "action" "set-skills")
  "scv arguments used to set the active Agent Skills."
  :type '(repeat string)
  :group 'scv)

(defcustom scv-session-action-interrupt-command
  '("session" "action" "interrupt")
  "scv arguments used to interrupt a running Coding Session."
  :type '(repeat string)
  :group 'scv)

(defcustom scv-session-action-permission-command
  '("session" "action" "permission")
  "scv arguments used to resolve a pending permission request."
  :type '(repeat string)
  :group 'scv)

(defcustom scv-prompt-display-action
  '(display-buffer-below-selected . ((window-height . 0.28)))
  "Display action for per-session Rich Prompt Box buffers."
  :type 'sexp
  :group 'scv)

(defvar scv--prompt-draft-buffers (make-hash-table :test #'equal)
  "Prompt draft buffers keyed by Session Origin and session id.")

(defvar scv--viewer-buffers (make-hash-table :test #'equal)
  "Session Viewer buffers keyed by Session Origin and session id.")

(defvar-local scv-session-id nil
  "Session id associated with the current scv buffer.")

(defvar-local scv-origin nil
  "Session Origin associated with the current scv buffer.")

(defvar-local scv--viewer-buffer nil
  "Session Viewer buffer associated with the current prompt draft.")

(defvar-local scv--prompt-submit-process nil
  "Latest prompt submission process for the current prompt draft.")

(defvar scv--last-prompt-failure-error nil
  "Last failing prompt submission summary for annoyed-quote cycling.")

(defvar scv--last-prompt-failure-time 0
  "Timestamp of last prompt submission failure for annoyed-quote cycling.")

(defvar scv--last-prompt-failure-count 0
  "Consecutive prompt submit failures for annoyed-quote cycling.")

;;; Utility

(defun scv--session-key (origin session-id)
  "Return a stable hash key for ORIGIN and SESSION-ID."
  (format "%s:%s" (or origin scv-default-origin) session-id))

(defun scv--built-executable ()
  "Return the source-tree built scv executable path."
  (let ((monorepo-built
         (expand-file-name "../_build/default/scv/bin/main.exe"
                           scv-source-directory))
        (legacy-built
         (expand-file-name "_build/default/bin/main.exe"
                           scv-source-directory)))
    (if (file-exists-p monorepo-built)
        monorepo-built
      legacy-built)))

(defun scv--executable ()
  "Return a scv executable path or signal an actionable error."
  (or (and scv-executable
           (file-executable-p scv-executable)
           scv-executable)
      (let ((built (scv--built-executable)))
        (when (file-executable-p built)
          built))
      (executable-find "scv")
      (user-error
       "scv is not built. Run `opam exec -- dune build @all` in %s"
       (abbreviate-file-name scv-source-directory))))

(defun scv--command (&rest args)
  "Return a scv process command list with ARGS."
  (append (list (scv--executable)) args))

(defun scv--integration-environment ()
  "Return a process environment with SCV Emacs integration flags enabled."
  (let ((env (copy-sequence process-environment)))
    (setenv "SCV_EMACS" "1" env)
    (setenv "SCV_SESSION_TEXT" "1" env)
    env))

(defun scv--shell-command (&rest args)
  "Return a shell-quoted scv command string with ARGS."
  (string-join (mapcar #'shell-quote-argument (apply #'scv--command args))
               " "))

(defun scv--require-session-id ()
  "Return the current Session id, prompting when none is associated."
  (or scv-session-id
      (read-string "Session id: ")))

(defun scv--origin ()
  "Return the current Session Origin."
  (or scv-origin scv-default-origin))

(defun scv--json-alist-get (key alist)
  "Return KEY from JSON ALIST using symbol or string keys."
  (or (alist-get key alist)
      (alist-get (symbol-name key) alist nil nil #'string=)))

(defun scv--json-field (row key &optional fallback)
  "Return string value for KEY in ROW, or FALLBACK."
  (let ((value (scv--json-alist-get key row)))
    (cond
     ((stringp value) value)
     ((numberp value) (number-to-string value))
     ((eq value t) "true")
     ((eq value :json-false) "false")
     ((null value) fallback)
     (t (format "%s" value)))))

(defun scv--call-process-string (args)
  "Run scv ARGS and return stdout as a string."
  (with-temp-buffer
    (let ((process-environment (scv--integration-environment))
          (status (apply #'process-file (scv--executable) nil t nil args)))
      (unless (eq status 0)
        (user-error "scv failed: %s" (string-trim (buffer-string))))
      (buffer-string))))

(defun scv--annoyed-quote (kind)
  "Return a SCV quote for KIND."
  (pcase kind
    ('rapid-failure "Whaddya want?!")
    ('invalid-command "You absolute mutt!")
    ('ctrl-c "Hey! You're stressin' me out here!")
    ('cpu-stress "Heck! Over-worked, under-paid, and under-armored!")
    ('token-limit "Not enough minerals.")
    ('context-limit "Not enough Vespene gas.")
    ('blocked "Can't build there, something's in the way.")
    (_ "What, you run out of Marines?")))

(defun scv--failure-kind (details)
  "Classify DETAILS to pick a failure quote."
  (let ((lower (downcase (or details ""))))
    (cond
     ((string-match-p "\\(invalid\\|unknown command\\|syntax\\|unknown option\\)"
                     lower)
      'invalid-command)
     ((string-match-p "context window\\|context limit\\|max tokens\\|rate limit"
                      lower)
      'token-limit)
     ((string-match-p "model context\\|token limit\\|too many tokens\\|not enough tokens"
                      lower)
      'context-limit)
     (t 'default))))

(defun scv--record-prompt-failure (details)
  "Record prompt failure details and return an annoyed quote."
  (let ((now (float-time))
        (current (and details (string-trim details))))
    (if (and current
             (string= current scv--last-prompt-failure-error)
             (< (- now scv--last-prompt-failure-time) 2.0))
        (setq scv--last-prompt-failure-count
              (1+ scv--last-prompt-failure-count))
      (setq scv--last-prompt-failure-count 1))
    (setq scv--last-prompt-failure-time now
          scv--last-prompt-failure-error current)
    (if (and (>= scv--last-prompt-failure-count 3)
             (string= current scv--last-prompt-failure-error))
        (scv--annoyed-quote 'rapid-failure)
      (scv--annoyed-quote (scv--failure-kind current)))))

(defun scv-generate-session-id ()
  "Return a fresh local scv Session id."
  (concat scv-session-id-prefix
          (substring
           (secure-hash 'sha1
                        (format "%s:%s:%s:%s"
                                (float-time)
                                (emacs-pid)
                                (random)
                                (user-uid)))
           0 16)))

(defun scv-reload ()
  "Reload the local scv Emacs integration checkout."
  (interactive)
  (load-file (expand-file-name "emacs/scv.el" scv-source-directory)))

;;; Session Manager

(defvar scv-session-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'scv-session-manager-open)
    (define-key map (kbd "g") #'scv-session-manager-refresh)
    map)
  "Keymap for `scv-session-manager-mode'.")

(define-derived-mode scv-session-manager-mode tabulated-list-mode
  "scv-sessions"
  "Major mode for listing scv Coding Sessions."
  (setq tabulated-list-format
        [("Origin" 12 t)
         ("Session" 30 t)
         ("State" 14 t)
         ("Viewer" 12 t)
         ("Project" 22 t)
         ("Title" 40 t)
         ("Last Activity" 24 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Last Activity" nil))
  (tabulated-list-init-header))

;;;###autoload
(defun scv-session-manager ()
  "Open the scv Session Manager."
  (interactive)
  (let ((buffer (get-buffer-create "*scv sessions*")))
    (with-current-buffer buffer
      (scv-session-manager-mode)
      (scv-session-manager-refresh))
    (pop-to-buffer buffer)))

(defun scv-session-manager--rows ()
  "Return raw JSON rows from the configured Session Listing Command."
  (let* ((json-array-type 'list)
         (json-object-type 'alist)
         (json-key-type 'symbol)
         (json-false :json-false)
         (output (scv--call-process-string scv-session-list-command))
         (parsed (json-read-from-string output)))
    (unless (listp parsed)
      (user-error "Session Listing Command returned a non-list JSON payload"))
    parsed))

(defun scv-session-manager--entry (row)
  "Return a tabulated-list entry for ROW."
  (let* ((session-id (or (scv--json-field row 'session_id)
                         (scv--json-field row 'id)
                         ""))
         (origin (or (scv--json-field row 'origin)
                     scv-default-origin))
         (state (or (scv--json-field row 'state) "-"))
         (viewer (or (scv--json-field row 'viewer_attachment_state)
                     (scv--json-field row 'viewer)
                     "-"))
         (project (or (scv--json-field row 'project) "-"))
         (title (or (scv--json-field row 'title) "-"))
         (last-activity (or (scv--json-field row 'last_activity)
                            (scv--json-field row 'last_activity_at)
                            "-")))
    (list (scv--session-key origin session-id)
          (vector origin session-id state viewer project title last-activity))))

(defun scv-session-manager-refresh ()
  "Refresh the Session Manager from the Session Listing Command."
  (interactive)
  (setq tabulated-list-entries
        (mapcar #'scv-session-manager--entry
                (scv-session-manager--rows)))
  (tabulated-list-print t))

(defun scv-session-manager--row-at-point ()
  "Return the selected Session Manager row."
  (let ((entry (tabulated-list-get-entry))
        (id (tabulated-list-get-id)))
    (unless entry
      (user-error "No session row at point"))
    (list :origin (aref entry 0)
          :session-id (aref entry 1)
          :key id)))

(defun scv-session-manager-open ()
  "Open a Session Viewer for the selected Session Manager row."
  (interactive)
  (let* ((row (scv-session-manager--row-at-point))
         (origin (plist-get row :origin))
         (session-id (plist-get row :session-id)))
    (scv-open-session-viewer session-id origin)))

;;; Prompt Draft

(defvar scv-prompt-draft-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'scv-prompt-submit)
    (define-key map (kbd "C-c C-f") #'scv-prompt-insert-file)
    (define-key map (kbd "C-c C-k") #'scv-prompt-clear)
    (define-key map (kbd "C-c C-v") #'scv-prompt-jump-to-viewer)
    (define-key map (kbd "/") #'scv-prompt-slash-command)
    map)
  "Keymap for `scv-prompt-draft-mode'.")

(define-minor-mode scv-prompt-draft-mode
  "Minor mode for a scv Rich Prompt Box."
  :lighter " tPrompt"
  :keymap scv-prompt-draft-mode-map)

(defun scv--prompt-draft-buffer-name (origin session-id)
  "Return the Rich Prompt Box buffer name for ORIGIN and SESSION-ID."
  (format "*scv prompt %s:%s*" origin session-id))

(defun scv--prompt-draft-buffer (session-id origin &optional viewer-buffer)
  "Return the prompt draft buffer for SESSION-ID and ORIGIN.

When VIEWER-BUFFER is non-nil, remember it as the associated Session Viewer."
  (let* ((origin (or origin scv-default-origin))
         (key (scv--session-key origin session-id))
         (existing (gethash key scv--prompt-draft-buffers)))
    (or (and (buffer-live-p existing)
             (progn
               (when (and viewer-buffer (not (eq viewer-buffer existing)))
                 (with-current-buffer existing
                   (setq-local scv--viewer-buffer viewer-buffer)))
               existing))
        (let ((buffer (get-buffer-create
                       (scv--prompt-draft-buffer-name origin session-id))))
          (puthash key buffer scv--prompt-draft-buffers)
          (with-current-buffer buffer
            (text-mode)
            (setq-local scv-session-id session-id)
            (setq-local scv-origin origin)
            (when viewer-buffer
              (setq-local scv--viewer-buffer viewer-buffer))
            (scv-prompt-draft-mode 1))
          buffer))))

(defun scv--display-prompt-draft-buffer (buffer)
  "Display BUFFER as a Rich Prompt Box and return its window."
  (or (display-buffer buffer scv-prompt-display-action)
      (display-buffer buffer)))

;;;###autoload
(defun scv-open-prompt-draft (&optional session-id origin)
  "Open the Rich Prompt Box for SESSION-ID and ORIGIN."
  (interactive)
  (let* ((session-id (or session-id (scv--require-session-id)))
         (origin (or origin (scv--origin)))
         (viewer (current-buffer))
         (buffer (scv--prompt-draft-buffer session-id origin viewer))
         (window (scv--display-prompt-draft-buffer buffer)))
    (select-window window)
    buffer))

(defun scv-prompt-clear ()
  "Clear the current Prompt Draft."
  (interactive)
  (unless scv-prompt-draft-mode
    (user-error "Not in a scv prompt draft"))
  (erase-buffer)
  (set-buffer-modified-p nil))

(defun scv-prompt-jump-to-viewer ()
  "Jump to the Session Viewer associated with the current Prompt Draft."
  (interactive)
  (unless (buffer-live-p scv--viewer-buffer)
    (user-error "This prompt draft has no live Session Viewer"))
  (pop-to-buffer scv--viewer-buffer))

(defun scv--prompt-submit-args (session-id kind)
  "Return scv argv for submitting SESSION-ID prompt of KIND."
  (append scv-session-action-submit-command
          (list "--session-id" session-id "--kind" kind)))

(defun scv--prompt-submit-command (session-id kind)
  "Return process command for submitting SESSION-ID prompt of KIND."
  (apply #'scv--command (scv--prompt-submit-args session-id kind)))

(defun scv--set-skills-args (session-id skills)
  "Return scv argv to set SKILLS for SESSION-ID."
  (append
   scv-session-action-set-skills-command
	   (append (list "--session-id" session-id)
	           (apply #'append
	                  (mapcar (lambda (skill)
	                            (list "--skill" skill))
	                          skills)))))

(defun scv--prompt-submit-sentinel (draft tick process event)
  "Handle prompt submission PROCESS EVENT for DRAFT at modification TICK."
  (when (memq (process-status process) '(exit signal))
    (when (and (string-prefix-p "killed" (downcase (or event "")))
               (not (eq (process-exit-status process) 0)))
      (message "%s" (scv--annoyed-quote 'ctrl-c)))
    (let ((status (process-exit-status process))
          (details (when (buffer-live-p (process-buffer process))
                     (with-current-buffer (process-buffer process)
                       (string-trim (buffer-string))))))
      (if (eq status 0)
          (progn
            (when (buffer-live-p draft)
              (with-current-buffer draft
                (when (= tick (buffer-chars-modified-tick))
                  (erase-buffer)
                  (set-buffer-modified-p nil)
                  (scv--close-prompt-draft-windows draft))))
            (message "Job finished.")
            (setq scv--last-prompt-failure-count 0))
        (let ((normalized
               (if (and details (not (string-empty-p details)))
                   details
                 (string-trim event))))
          (message (scv--record-prompt-failure normalized)))))))

(defun scv-prompt-submit-kind (kind)
  "Submit the current Prompt Draft using KIND."
  (unless scv-prompt-draft-mode
    (user-error "Not in a scv prompt draft"))
  (let ((prompt (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (when (string-empty-p prompt)
      (user-error "Prompt draft is empty"))
    (let* ((session-id (scv--require-session-id))
           (command (scv--prompt-submit-command session-id kind))
           (error-buffer (get-buffer-create
                          (format "*scv prompt submit %s*" session-id)))
           (draft (current-buffer))
           (tick (buffer-chars-modified-tick))
           (process
            (let ((process-environment (scv--integration-environment)))
              (make-process
               :name (format "scv-prompt-submit-%s" session-id)
               :buffer error-buffer
               :command command
               :connection-type 'pipe
               :noquery t
               :sentinel
               (lambda (process event)
                 (scv--prompt-submit-sentinel draft tick process event))))))
      (setq-local scv--prompt-submit-process process)
      (process-send-string process prompt)
      (process-send-eof process)
      (message "Right away, sir."))))

(defun scv-prompt-submit (&optional follow-up)
  "Submit the current Prompt Draft.

With FOLLOW-UP, submit as an explicit follow-up prompt.  Otherwise the backend
uses its default prompt-kind policy for the Coding Session state."
  (interactive "P")
  (scv-prompt-submit-kind (if follow-up "follow-up" "auto")))

(defun scv-prompt-submit-follow-up ()
  "Submit the current Prompt Draft as an explicit follow-up prompt."
  (interactive)
  (scv-prompt-submit-kind "follow-up"))

;;; Prompt Draft: Convenience

(defun scv-prompt-insert-file ()
  "Insert the contents of a file into the current Prompt Draft."
  (interactive)
  (unless scv-prompt-draft-mode
    (user-error "Not in a scv prompt draft"))
  (let* ((file (read-file-name "Insert file: "))
         (content (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string))))
    (save-excursion
      (goto-char (point-max))
      (insert "\n```" (file-name-extension file) "\n")
      (insert content)
      (unless (string-suffix-p "\n" content)
        (insert "\n"))
      (insert "```\n"))
    (message "Inserted %s" file)))

(defun scv-prompt-slash-command ()
  "Complete a slash command at the beginning of the Prompt Draft."
  (interactive)
  (unless scv-prompt-draft-mode
    (user-error "Not in a scv prompt draft"))
  (cond
   ((not (bobp))
    ;; Not at beginning — insert literal slash
    (insert "/"))
   (t
    (let ((choice (completing-read
                   "scv command: "
                   '(("file" . "Insert file contents into prompt")
                     ("image" . "Reference an image file")
                     ("model" . "Set the active model")
                     ("skills" . "Set active Agent Skills")
                     ("clear" . "Clear the prompt draft"))
                   nil t)))
      (pcase choice
        ("file" (scv-prompt-insert-file))
        ("image"
         (let* ((file (read-file-name "Image file: "))
                (rel (file-relative-name file)))
           (save-excursion
             (goto-char (point-max))
             (insert (format "![image](%s)" rel)))))
        ("model"
         (let ((model (read-string "Model: ")))
           (save-excursion
             (goto-char (point-max))
             (insert (format "/model %s" model)))))
        ("skills"
         (let* ((skills-str (read-string "Skills (comma-separated): "))
                (skills (split-string skills-str "," t " ")))
           (save-excursion
             (goto-char (point-max))
             (insert (format "/skills %s" (string-join skills ","))))))
        ("clear" (scv-prompt-clear))
        (_ (message "Unknown command: %s" choice)))))))

;;; Session Viewer

(defun scv--open-link-target (link)
  "Open LINK by delegating URLs to the browser and file links to Emacs files.

URLs are opened through `browse-url`; file links support `file://` prefixes and
optional `:LINE` suffixes in the same plain-text forms emitted by the Session
Client (for example `README.md:128`)."
  (cond
   ((string-match-p "\\`https?://[^[:space:]]+\\'" link)
    (browse-url link))
   ((string-match "^file://\\([^#\n\r\t]*\\)\\(?:#L\\([0-9]+\\)\\)?$" link)
    (let* ((path (match-string 1 link))
           (line-number
            (and (match-string 2 link) (string-to-number (match-string 2 link)))))
      (find-file path)
      (when line-number
        (goto-char (point-min))
        (forward-line (max 0 (1- line-number))))))
   ((string-match "\\`\\(/\\|\\(?:\\.\\.?/\\)\\)[^[:space:]\n\r\t]*\\(:[0-9]+\\)?\\'" link)
    (let ((path link)
          line-number)
      (when (string-match "\\`\\(.*\\):\\([0-9]+\\)\\'" link)
        (setq path (match-string 1 link))
        (setq line-number (string-to-number (match-string 2 link))))
      (find-file (expand-file-name path default-directory))
      (when line-number
        (goto-char (point-min))
        (forward-line (max 0 (1- line-number))))))
   (t (user-error "No supported link at point: %s" link))))

(defun scv--extract-link-at-point ()
  "Return the nearest OSC-like URL or file token at point."
  (let ((line-start (line-beginning-position))
        (line-end (line-end-position))
        (cursor (point))
        found
        match-start
        match-end)
    (save-excursion
      (goto-char line-start)
      (while (and (not found)
                  (re-search-forward
                   "\\(?:https?://[^[:space:]\n\r\t]+\\|file://[^[:space:]\n\r\t]+\\|\\(?:/\\|\\.\\.?/\\)[^[:space:]\n\r\t]+:[0-9]+\\|\\(?:/\\|\\.\\.?/\\)[^[:space:]\n\r\t]+\\)"
                   line-end t))
        (setq match-start (match-beginning 0))
        (setq match-end (match-end 0))
        (when (and (<= match-start cursor) (<= cursor match-end))
          (setq found (buffer-substring-no-properties match-start match-end)))))
    found))

(defun scv-session-viewer-open-link-at-point ()
  "Open a link from the Session Viewer output at point.

File links are opened in Emacs, URL links with `browse-url`."
  (interactive)
  (let ((link (scv--extract-link-at-point)))
    (unless link (user-error "No link at point"))
    (scv--open-link-target link)))

(defun scv--close-prompt-draft-windows (buffer)
  "Close windows displaying Prompt Draft BUFFER when possible."
  (dolist (window (get-buffer-window-list buffer nil t))
    (when (window-live-p window)
      (ignore-errors
        (delete-window window)))))

;;;###autoload
(defun scv-open-session-viewer (session-id &optional origin)
  "Open a native Emacs Session Viewer for SESSION-ID and ORIGIN."
  (interactive (list (read-string "Session id: ")))
  (message "SCV good to go, sir!")
  (let* ((origin (or origin scv-default-origin))
         (key (scv--session-key origin session-id))
         (existing (gethash key scv--viewer-buffers)))
    (if (buffer-live-p existing)
        (progn
          (message "Delivered!")
          (pop-to-buffer existing))
      (unless (string-equal origin scv-default-origin)
        (user-error "Only the local Session Origin is implemented in this viewer"))
      (require 'scv-session-viewer)
      (scv-session-viewer-open session-id)
      (message "Delivered!"))))

;;;###autoload
(defun scv-new-session (&optional prompt-for-id)
  "Start a new local Session Viewer.

With PROMPT-FOR-ID, prompt for the Session id instead of using a generated id."
  (interactive "P")
  (let ((session-id (if prompt-for-id
                        (read-string "Session id: "
                                     (scv-generate-session-id))
                      (scv-generate-session-id))))
    (scv--call-process-string
     (list "session" "--session-id" session-id "--detach" "--json"))
    (scv-open-session-viewer session-id scv-default-origin)
    session-id))

;;; Menu

(defun scv--fallback-menu ()
  "Read and dispatch one scv menu key without `transient'."
  (pcase (read-key "scv: [n]ew [s]essions [v]iewer [p]rompt [r]eload ")
    (?n (call-interactively #'scv-new-session))
    (?s (call-interactively #'scv-session-manager))
    (?v (call-interactively #'scv-open-session-viewer))
    (?p (call-interactively #'scv-open-prompt-draft))
    (?r (call-interactively #'scv-reload))
    (_ (user-error "Unknown scv menu key"))))

(when (featurep 'transient)
  (transient-define-prefix scv--transient-menu ()
    "scv command menu."
    [["Sessions"
      ("n" "new" scv-new-session)
      ("s" "manager" scv-session-manager)
      ("v" "viewer" scv-open-session-viewer)]
     ["Prompt"
      ("p" "draft" scv-open-prompt-draft)]
     ["Development"
      ("r" "reload" scv-reload)]]))

;;;###autoload
(defun scv-menu ()
  "Open the scv transient command menu."
  (interactive)
  (if (fboundp 'scv--transient-menu)
      (scv--transient-menu)
    (scv--fallback-menu)))

;;; Install

;;;###autoload
(defun scv-install (leader-map &optional key)
  "Attach scv key bindings to LEADER-MAP under KEY.

KEY defaults to \"S\"."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") #'scv-menu)
    (define-key map (kbd "n") #'scv-new-session)
    (define-key map (kbd "s") #'scv-session-manager)
    (define-key map (kbd "v") #'scv-open-session-viewer)
    (define-key map (kbd "p") #'scv-open-prompt-draft)
    (define-key leader-map (kbd (or key "S")) map)))

(provide 'scv)
;;; scv.el ends here
