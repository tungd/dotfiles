;;; td-acp-tests.el --- Tests for td-acp.el -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; ERT coverage for the Org-backed ACP UI wrapper.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'project)

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'td-acp)

(defmacro td-acp-test-with-project (bindings &rest body)
  "Run BODY inside a temporary project directory.

BINDINGS must bind a single symbol to the temp directory path."
  (declare (indent 1))
  `(let ((,(car bindings) (make-temp-file "td-acp-test-" t)))
     (unwind-protect
         (let ((default-directory ,(car bindings))
               (td-acp--sessions (make-hash-table :test #'equal))
               (td-acp--pending-sessions nil)
               (td-acp--terminals (make-hash-table :test #'equal))
               (td-acp--terminal-counter 0))
           (make-directory (expand-file-name ".git" ,(car bindings)) t)
           ,@body)
       (ignore-errors
         (delete-directory ,(car bindings) t)))))

(defmacro td-acp-test-with-stubbed-acp (vars &rest body)
  "Bind ACP stubs and execute BODY.

VARS should be a plist-like list of symbols to be bound:
REQUESTS, RESPONSES, NOTIFICATIONS, ERRORS, DISPATCHER, NOTIF-HANDLER, and
REQUEST-HANDLER."
  (declare (indent 1))
  (let ((requests (plist-get vars :requests))
        (responses (plist-get vars :responses))
        (notifications (plist-get vars :notifications))
        (errors (plist-get vars :errors))
        (dispatcher (plist-get vars :dispatcher))
        (notif-handler (plist-get vars :notif-handler))
        (request-handler (plist-get vars :request-handler)))
    `(cl-letf (((symbol-function 'acp-make-client)
                (lambda (&rest args)
                  (list :fake-client t :args args)))
               ((symbol-function 'acp-subscribe-to-notifications)
                (lambda (&rest args)
                  (setq ,notif-handler (plist-get args :on-notification))))
               ((symbol-function 'acp-subscribe-to-requests)
                (lambda (&rest args)
                  (setq ,request-handler (plist-get args :on-request))))
               ((symbol-function 'acp-subscribe-to-errors)
                (lambda (&rest args)
                  (setq ,errors (plist-get args :on-error))))
               ((symbol-function 'acp-send-request)
                (lambda (&rest args)
                  (let* ((request (plist-get args :request))
                         (method (or (alist-get :method request)
                                     (alist-get 'method request)))
                         (on-success (plist-get args :on-success))
                         (on-failure (plist-get args :on-failure)))
                    (push request ,requests)
                    (pcase (funcall ,dispatcher method request)
                      (`(:success . ,result)
                       (funcall on-success result))
                      (`(:failure . ,err)
                       (funcall on-failure err))
                      (_
                       nil)))))
               ((symbol-function 'acp-send-notification)
                (lambda (&rest args)
                  (push (plist-get args :notification) ,notifications)))
               ((symbol-function 'acp-send-response)
                (lambda (&rest args)
                  (push (plist-get args :response) ,responses)))
               ((symbol-function 'acp-shutdown)
                (lambda (&rest _args) t))
               ((symbol-function 'acp-make-session-new-request)
                (lambda (&rest args)
                  `((:method . "session/new")
                    (:params . ((cwd . ,(plist-get args :cwd)))))))
               ((symbol-function 'acp-make-session-load-request)
                (lambda (&rest args)
                  `((:method . "session/load")
                    (:params . ((sessionId . ,(plist-get args :session-id))
                                (cwd . ,(plist-get args :cwd)))))))
               ((symbol-function 'acp-make-session-cancel-notification)
                (lambda (&rest args)
                  `((:method . "session/cancel")
                    (:params . ((sessionId . ,(plist-get args :session-id))
                                (reason . ,(plist-get args :reason)))))))
               ((symbol-function 'acp-make-session-request-permission-response)
                (lambda (&rest args)
                  `((:request-id . ,(plist-get args :request-id))
                    (:result . ((outcome . ,(if (plist-get args :cancelled)
                                                '((outcome . "cancelled"))
                                              `((outcome . "selected")
                                                (optionId . ,(plist-get args :option-id))))))))))
               ((symbol-function 'acp-make-fs-read-text-file-response)
                (lambda (&rest args)
                  `((:request-id . ,(plist-get args :request-id))
                    ,@(when (plist-member args :content)
                        `((:result . ((content . ,(plist-get args :content))))))
                    ,@(when (plist-member args :error)
                        `((:error . ,(plist-get args :error)))))))
               ((symbol-function 'acp-make-fs-write-text-file-response)
                (lambda (&rest args)
                  `((:request-id . ,(plist-get args :request-id))
                    ,@(when (plist-member args :error)
                        `((:error . ,(plist-get args :error))))
                    ,@(unless (plist-member args :error)
                        `((:result . nil))))))
               ((symbol-function 'acp-make-error)
                (lambda (&rest args)
                  `((code . ,(plist-get args :code))
                    (message . ,(plist-get args :message))
                    (data . ,(plist-get args :data))))))
       ,@body)))

(defun td-acp-test--session-file (project-root session-id)
  "Return transcript path for PROJECT-ROOT and SESSION-ID."
  (expand-file-name (format ".agents/sessions/%s.org" session-id) project-root))

(defun td-acp-test--write-transcript (path)
  "Write a minimal transcript to PATH."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert "#+TITLE: Existing Session\n"
            "#+PROPERTY: TD_ACP_SESSION_ID session-old\n"
            "#+PROPERTY: TD_ACP_CREATED_AT 2026-03-09T00:00:00+0700\n"
            "#+PROPERTY: TD_ACP_UPDATED_AT 2026-03-09T00:00:00+0700\n"
            "#+PROPERTY: TD_ACP_AGENT_COMMAND test-agent\n"
            "#+PROPERTY: TD_ACP_LOAD_SESSION true\n\n"
            "* User: hi\n\n"
            "* Agent\n"
            "hello\n")))

(ert-deftest td-acp-session-new-bootstraps-and-persists ()
  (td-acp-test-with-project (project-root)
    (let (requests responses notifications errors dispatcher notif-handler request-handler)
      (setq dispatcher
            (lambda (method _request)
              (pcase method
                ("initialize"
                 '(:success . ((agentCapabilities . ((loadSession . t))))))
                ("session/new"
                 '(:success . ((sessionId . "session-1"))))
                (_ '(:failure . ((message . "unexpected")))))))
      (td-acp-test-with-stubbed-acp
          (:requests requests
           :responses responses
           :notifications notifications
           :errors errors
           :dispatcher dispatcher
           :notif-handler notif-handler
           :request-handler request-handler)
        (td-acp-session-new)
        (let* ((session (gethash "session-1" td-acp--sessions))
               (file (td-acp-test--session-file project-root "session-1")))
          (should session)
          (should (buffer-live-p (td-acp-session-prompt-buffer session)))
          (should (file-exists-p file))
          (should (equal (td-acp-session-status session) "idle"))
          (should (equal (length requests) 2))
          (with-temp-buffer
            (insert-file-contents file)
            (should (search-forward "TD_ACP_SESSION_ID session-1" nil t))
            (should (search-forward "* User" nil t))
            (should (search-forward "* Agent" nil t))
            (should-not (search-forward "#+begin_src text" nil t))))))))

(ert-deftest td-acp-session-update-persists-turn-data ()
  (td-acp-test-with-project (project-root)
    (let* ((session (td-acp--make-session project-root
                                          '(:session-id "session-2"
                                            :title "Session Two")))
           (file (td-acp-test--session-file project-root "session-2")))
      (setf (td-acp-session-id session) "session-2")
      (setf (td-acp-session-transcript-file session) file)
      (td-acp--apply-session-update
       session
       '((sessionUpdate . "agent_message_chunk")
         (content . [((type . "text") (text . "hello"))])))
      (td-acp--apply-session-update
       session
       '((sessionUpdate . "agent_thought_chunk")
         (content . [((type . "text") (text . "thinking"))])))
      (td-acp--apply-session-update
       session
       '((sessionUpdate . "tool_call")
         (toolCallId . "task-1")
         (title . "Explore crash game codebase")
         (kind . "think")
         (status . "pending")
         (content . [((type . "content")
                      (content . ((type . "text")
                                  (text . "Look at build files"))))])))
      (td-acp--apply-session-update
       session
       '((sessionUpdate . "tool_call")
         (toolCallId . "tool-1")
         (title . "Read file")
         (kind . "read")
         (status . "completed")
         (content . [((type . "content")
                      (content . ((type . "text") (text . "done"))))])))
      (with-temp-buffer
        (insert-file-contents file)
        (should (search-forward "hello" nil t))
        (goto-char (point-min))
        (should (search-forward "** Thoughts" nil t))
        (should (search-forward "thinking" nil t))
        (goto-char (point-min))
        (should (search-forward "** Tasks" nil t))
        (should (search-forward "*** Explore crash game codebase" nil t))
        (should (search-forward ":TOOL_CALL_ID: task-1" nil t))
        (goto-char (point-min))
        (should (search-forward "** Tool calls" nil t))
        (should (search-forward "*** Read file" nil t))
        (should (search-forward ":TOOL_CALL_ID: tool-1" nil t))
        (should (search-forward "Content:" nil t))
        (should (search-forward "#+begin_src text" nil t))))))

(ert-deftest td-acp-open-existing-session-loads-when-supported ()
  (td-acp-test-with-project (project-root)
    (let* ((file (td-acp-test--session-file project-root "session-old"))
           original requests responses notifications errors dispatcher notif-handler request-handler)
      (td-acp-test--write-transcript file)
      (setq original (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string)))
      (setq dispatcher
            (lambda (method _request)
              (pcase method
                ("initialize"
                 '(:success . ((agentCapabilities . ((loadSession . t))))))
                ("session/load"
                 '(:success . ((configOptions . [])
                               (modes . ((currentModeId . "default"))))))
                (_ '(:failure . ((message . "unexpected")))))))
      (td-acp-test-with-stubbed-acp
          (:requests requests
           :responses responses
           :notifications notifications
           :errors errors
           :dispatcher dispatcher
           :notif-handler notif-handler
           :request-handler request-handler)
        (let* ((metadata (td-acp--session-from-file file))
               (session (td-acp-session-open metadata)))
          (should (equal (td-acp-session-status session) "connected"))
          (should (td-acp-session-client session))
          (should (equal (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         original))
          (funcall notif-handler
                   '((method . "session/update")
                     (params . ((sessionId . "session-old")
                                (update . ((sessionUpdate . "agent_message_chunk")
                                           (content . [((type . "text")
                                                        (text . "restored"))])))))))
          (with-temp-buffer
            (insert-file-contents file)
            (should (search-forward "restored" nil t))))))))

(ert-deftest td-acp-open-existing-session-falls-back-to-history-only ()
  (td-acp-test-with-project (project-root)
    (let* ((file (td-acp-test--session-file project-root "session-old")))
      (td-acp-test--write-transcript file)
      (with-temp-file file
        (insert "#+TITLE: Existing Session\n"
                "#+PROPERTY: TD_ACP_SESSION_ID session-old\n"
                "#+PROPERTY: TD_ACP_LOAD_SESSION false\n"))
      (let ((session (td-acp-session-open (td-acp--session-from-file file))))
        (should (equal (td-acp-session-status session) "history-only"))
        (should-not (td-acp-session-client session))))))

(ert-deftest td-acp-read-text-file-prefers-unsaved-buffer ()
  (td-acp-test-with-project (project-root)
    (let ((file (expand-file-name "notes.txt" project-root)))
      (with-temp-file file
        (insert "one\ntwo\nthree\n"))
      (with-current-buffer (find-file-noselect file)
        (erase-buffer)
        (insert "one\nchanged\nthree\n")
        (should (equal (td-acp--read-text-file file 2 1) "changed"))
        (kill-buffer (current-buffer))))))

(ert-deftest td-acp-write-text-file-updates-buffer-and-disk ()
  (td-acp-test-with-project (project-root)
    (let ((file (expand-file-name "edit.txt" project-root)))
      (with-temp-file file
        (insert "before"))
      (with-current-buffer (find-file-noselect file)
        (td-acp--write-text-file file "after")
        (should (equal (buffer-string) "after"))
        (kill-buffer (current-buffer)))
      (with-temp-buffer
        (insert-file-contents file)
        (should (equal (buffer-string) "after"))))))

(ert-deftest td-acp-terminal-lifecycle-responds-and-persists ()
  (td-acp-test-with-project (project-root)
    (let* ((session (td-acp--make-session project-root
                                          '(:session-id "session-term"
                                            :title "Terminal")))
           (responses nil)
           (file (td-acp-test--session-file project-root "session-term")))
      (setf (td-acp-session-id session) "session-term")
      (setf (td-acp-session-transcript-file session) file)
      (cl-letf (((symbol-function 'acp-send-response)
                 (lambda (&rest args)
                   (push (plist-get args :response) responses))))
        (td-acp--terminal-create
         session
         '((command . "sh")
           (args . ["-c" "printf hi"])
           (cwd . nil)
           (outputByteLimit . 1024))
         1)
        (let* ((create-response (car responses))
               (terminal-id (alist-get 'terminalId (alist-get :result create-response))))
          (should terminal-id)
          (while (process-live-p (td-acp-terminal-process (gethash terminal-id td-acp--terminals)))
            (accept-process-output nil 0.05))
          (setq responses nil)
          (td-acp--terminal-output session `((terminalId . ,terminal-id)) 2)
          (should (string-match-p "hi"
                                  (alist-get 'output
                                             (alist-get :result (car responses)))))
          (setq responses nil)
          (td-acp--terminal-wait-for-exit session `((terminalId . ,terminal-id)) 3)
          (should (alist-get 'exitCode (alist-get :result (car responses))))
          (td-acp--terminal-release session `((terminalId . ,terminal-id)) 4)
          (with-temp-buffer
            (insert-file-contents file)
            (should (search-forward "Created sh -c printf hi" nil t))
            (should (search-forward "Released" nil t))))))))

(ert-deftest td-acp-permission-request-responds-and-persists ()
  (td-acp-test-with-project (project-root)
    (let* ((session (td-acp--make-session project-root
                                          '(:session-id "session-perm"
                                            :title "Permissions")))
           (responses nil)
           (file (td-acp-test--session-file project-root "session-perm"))
           (td-acp-permission-policy 'ask-risky)
           (completion-collection nil)
           (completion-prompt nil))
      (setf (td-acp-session-id session) "session-perm")
      (setf (td-acp-session-transcript-file session) file)
      (cl-letf (((symbol-function 'acp-send-response)
                 (lambda (&rest args)
                   (push (plist-get args :response) responses)))
                ((symbol-function 'completing-read)
                 (lambda (prompt collection &rest _args)
                   (setq completion-prompt prompt)
                   (setq completion-collection collection)
                   (caar collection))))
        (td-acp--handle-request
         session
         '((id . 11)
           (method . "session/request_permission")
           (params . ((message . "Choose execution permission")
                      (options . [((optionId . "allow_always")
                                   (name . "Always Allow")
                                   (kind . "allow_always"))
                                  ((optionId . "allow_once")
                                   (name . "Allow once")
                                   (kind . "allow_once")
                                   (description . "Run this command one time"))
                                  ((optionId . "reject")
                                   (name . "Reject")
                                   (kind . "reject"))])
                      (toolCall . ((title . "Run command")
                                   (rawInput . ((command . "find /tmp -name \"*.yml\"")
                                                (description . "List YAML files")))))))))
        (should (equal completion-prompt
                       "Choose execution permission for `find /tmp -name \"*.yml\"`: "))
        (should (equal (caar completion-collection)
                       "Always Allow - [allow_always]"))
        (should (equal (alist-get 'outcome (alist-get :result (car responses)))
                       "selected"))
        (should (equal (alist-get 'optionId (alist-get :result (car responses)))
                       "allow_always"))
        (with-temp-buffer
          (insert-file-contents file)
          (should (search-forward "** Permissions" nil t))
          (should (search-forward "allow_always" nil t)))))))

(provide 'td-acp-tests)

;;; td-acp-tests.el ends here
