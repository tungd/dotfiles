;;; scv-session-viewer.el --- Native Emacs Session Client -*- lexical-binding: t; -*-

;; Renders a Coding Session into an Emacs buffer by reading the Session Log
;; (JSONL), projecting events through scv-reducer, and connecting to the
;; Session Host via Unix socket for live updates.
;;
;; Package-Requires: ((emacs "27.1") (cl-lib "1.0") (json "2.0") (seq "2.0"))

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'scv)
(require 'scv-reducer)

;;; Faces

(defface scv-viewer-user
  '((t :foreground "#61afef" :weight bold))
  "Face for user prompts in the Session Viewer."
  :group 'scv)

(defface scv-viewer-assist
  '((t :foreground "#abb2bf"))
  "Face for assistant responses."
  :group 'scv)

(defface scv-viewer-tool-run
  '((t :foreground "#e5c07b"))
  "Face for running tools."
  :group 'scv)

(defface scv-viewer-tool-ok
  '((t :foreground "#98c379"))
  "Face for successful tools."
  :group 'scv)

(defface scv-viewer-tool-err
  '((t :foreground "#e06c75"))
  "Face for failed tools."
  :group 'scv)

(defface scv-viewer-blocker
  '((t :foreground "#c678dd" :weight bold))
  "Face for pending blockers."
  :group 'scv)

(defface scv-viewer-footer
  '((t :foreground "#5c6370"))
  "Face for footer text."
  :group 'scv)

(defface scv-viewer-header
  '((t :foreground "#e5c07b" :weight bold))
  "Face for session header."
  :group 'scv)

(defface scv-viewer-prompt
  '((t :foreground "#61afef" :weight bold))
  "Face for the input prompt area."
  :group 'scv)

(defface scv-viewer-st-idle
  '((t :foreground "#98c379"))
  "Face for idle/completed state."
  :group 'scv)

(defface scv-viewer-st-run
  '((t :foreground "#e5c07b"))
  "Face for running state."
  :group 'scv)

(defface scv-viewer-st-block
  '((t :foreground "#c678dd"))
  "Face for blocked state."
  :group 'scv)

(defface scv-viewer-st-err
  '((t :foreground "#e06c75"))
  "Face for error/interrupted state."
  :group 'scv)

;;; Paths

(defun scv-session-viewer--log-path (session-id)
  "Return the JSONL Session Log path for SESSION-ID."
  (expand-file-name
   (concat "transcripts/" session-id ".jsonl")
   (or (getenv "SCV_DATA_DIR")
       (expand-file-name ".scv" (getenv "HOME")))))

(defun scv-session-viewer--socket-path (session-id)
  "Return the Session Host Unix socket path for SESSION-ID."
  (expand-file-name
   (concat "session-client/" session-id ".socket")
   (or (getenv "SCV_RUNTIME_DIR")
       (expand-file-name ".scv/runtime" (getenv "HOME")))))

;;; JSONL Parser

(defun scv-session-viewer--parse-line (line)
  "Parse one JSONL LINE into a decoded event alist, or nil on failure."
  (when (and line (not (string-empty-p line)))
    (condition-case nil
        (json-parse-string line :object-type 'alist
                           :array-type 'list
                           :null-object nil
                           :false-object :json-false)
      (error nil))))

(defun scv-session-viewer--read-all-events (log-path)
  "Read ALL events from LOG-PATH. Returns (EVENTS . BYTE-POS)."
  (if (not (file-exists-p log-path))
      (cons nil 0)
    (let ((events nil)
          (file-size 0))
      (with-temp-buffer
        (insert-file-contents log-path)
        (setq file-size (buffer-size))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (forward-line 1)
            (let ((event (scv-session-viewer--parse-line line)))
              (when event (push event events))))))
      (cons (nreverse events) file-size))))

(defun scv-session-viewer--read-new-events (log-path last-byte-pos)
  "Read new JSONL lines from LOG-PATH starting after LAST-BYTE-POS.
Returns (EVENTS . NEW-BYTE-POS)."
  (if (not (file-exists-p log-path))
      (cons nil last-byte-pos)
    (let* ((attrs (file-attributes log-path))
           (file-size (nth 7 attrs)))
      (cond
       ((< file-size last-byte-pos)
        ;; File was truncated, read from beginning
        (scv-session-viewer--read-all-events log-path))
       ((or (null file-size) (<= file-size last-byte-pos))
        (cons nil last-byte-pos))
       (t
        (let ((events nil)
              (partial "")
              (pos last-byte-pos))
          (with-temp-buffer
            (insert-file-contents log-path nil last-byte-pos file-size)
            (goto-char (point-min))
            (while (not (eobp))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
                    (line-end-pos (line-end-position)))
                (forward-line 1)
                (let ((full (concat partial line)))
                  (if (eobp)
                      ;; Last line may be incomplete
                      (when (> (length full) 0)
                        (setq partial full))
                    ;; Complete line
                    (let ((event (scv-session-viewer--parse-line full)))
                      (when event (push event events)))
                    (setq partial "")
                    (setq pos (1- (point))))))))
          (cons (nreverse events) pos)))))))

;;; Render helpers (return strings for testing)

(defun scv-session-viewer--state-label (state)
  "Human-readable label for STATE symbol."
  (pcase state
    ('created "created")
    ('idle "idle")
    ('running "running")
    ('prompt_queued "prompt queued")
    ('blocked_on_permission "blocked: permission")
    ('blocked_on_question "blocked: question")
    ('handing_off "handing off")
    ('interrupted "interrupted")
    ('completed "completed")
    ('failed "failed")
    (_ (format "%s" state))))

(defun scv-session-viewer--state-face (state)
  "Status face for STATE."
  (pcase state
    ((or 'idle 'completed) 'scv-viewer-st-idle)
    ((or 'running 'prompt_queued) 'scv-viewer-st-run)
    ((or 'blocked_on_permission 'blocked_on_question 'handing_off) 'scv-viewer-st-block)
    ((or 'failed 'interrupted) 'scv-viewer-st-err)
    (_ 'scv-viewer-footer)))

(defun scv-session-viewer--render-header-text (proj)
  "Return header text for PROJ."
  (with-temp-buffer
    (insert (format "session: %s\n"
                    (or (scv-projection-session-id proj) "-")))
    (insert (format "model: %s | permission: %s\n"
                    (or (scv-projection-model proj) "-")
                    (or (scv-projection-permission-mode proj) '-)))
    (when (scv-projection-task-title proj)
      (insert (format "title: %s\n" (scv-projection-task-title proj))))
    (insert "\n")
    (buffer-string)))

(defun scv-session-viewer--render-messages-text (proj)
  "Return messages text for PROJ."
  (with-temp-buffer
    (dolist (msg (scv-projection-messages proj))
      (let ((start (point)))
        (insert (format ">>> %s\n" (scv-message-role msg)))
        (insert (scv-message-content msg))
        (insert "\n\n")
        (put-text-property start (point) 'face
                           (pcase (scv-message-role msg)
                             ("user" 'scv-viewer-user)
                             ("assistant" 'scv-viewer-assist)
                             (_ 'scv-viewer-footer)))))
    (buffer-string)))

(defun scv-session-viewer--render-tools-text (proj)
  "Return tools text for PROJ."
  (with-temp-buffer
    (let ((tools (scv-projection-tools proj)))
      (when tools
        (insert "--- tools ---\n")
        (dolist (tool tools)
          (let ((start (point)))
            (insert (format "[%s] %s %s"
                            (pcase (scv-tool-status tool)
                              ("running" "…")
                              ("success" "✓")
                              ("failure" "✗")
                              (_ "?"))
                            (scv-tool-name tool)
                            (scv-tool-input-preview tool)))
            (when (and (not (string-empty-p (scv-tool-result tool)))
                       (member (scv-tool-status tool) '("success" "failure")))
              (insert (format " → %s"
                              (truncate-string-to-width
                               (scv-tool-result tool) 80 nil nil "…"))))
            (insert "\n")
            (put-text-property start (point) 'face
                               (pcase (scv-tool-status tool)
                                 ("running" 'scv-viewer-tool-run)
                                 ("success" 'scv-viewer-tool-ok)
                                 ("failure" 'scv-viewer-tool-err)
                                 (_ 'scv-viewer-footer)))))
        (insert "\n")))
    (buffer-string)))

(defun scv-session-viewer--render-blockers-text (proj)
  "Return blockers text for PROJ."
  (with-temp-buffer
    (let ((pending (cl-remove-if
                    (lambda (b)
                      (not (string= (scv-blocker-status b) "pending")))
                    (scv-projection-blockers proj))))
      (when pending
        (insert "--- blockers ---\n")
        (dolist (b pending)
          (let ((start (point)))
            (insert (format "[%s] %s: %s\n"
                            (scv-blocker-id b)
                            (if (eq (scv-blocker-type b) 'permission)
                                (scv-blocker-tool-name b)
                              "question")
                            (scv-blocker-detail b)))
            (put-text-property start (point) 'face 'scv-viewer-blocker)))
        (insert "\n")))
    (buffer-string)))

(defun scv-session-viewer--render-footer-text (proj)
  "Return footer text for PROJ."
  (format "[%s]\n"
          (scv-session-viewer--state-label
           (scv-projection-state proj))))

(defun scv-session-viewer--render-input-area-text ()
  "Return the input prompt area text."
  (concat "─" (make-string 60 ?─) "\n" "start typing to prompt > "))

(defun scv-session-viewer--render-handoff-text (proj)
  "Return handoff progress text for PROJ."
  (with-temp-buffer
    (let ((handoffs (scv-projection-handoff-progress proj)))
      (when handoffs
        (insert "--- handoff ---\n")
        (dolist (h handoffs)
          (insert (format "  [%s] %s: %s\n"
                          (scv-handoff-stage h)
                          (or (scv-handoff-event-type h) "handoff")
                          (truncate-string-to-width
                           (scv-handoff-text h) 80 nil nil "…"))))
        (insert "\n")))
    (buffer-string)))

(defun scv-session-viewer--render-full-text (proj)
  "Return full rendered text for PROJ."
  (with-temp-buffer
    (insert (scv-session-viewer--render-header-text proj))
    (insert (scv-session-viewer--render-blockers-text proj))
    (insert (scv-session-viewer--render-messages-text proj))
    (insert (scv-session-viewer--render-tools-text proj))
    (insert (scv-session-viewer--render-handoff-text proj))
    (insert (scv-session-viewer--render-footer-text proj))
    (insert (scv-session-viewer--render-input-area-text))
    (buffer-string)))

;;; Buffer Renderer

(defun scv-session-viewer--erase-footer ()
  "Remove footer and input area from end of buffer."
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward "\n─" nil t)
      (delete-region (point) (point-max)))))

(defun scv-session-viewer--render-full (proj)
  "Full re-render of PROJ into current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (scv-session-viewer--render-header-text proj)
                        'face 'scv-viewer-header))
    (insert (propertize (scv-session-viewer--render-blockers-text proj)
                        'face 'scv-viewer-blocker))
    (dolist (msg (scv-projection-messages proj))
      (let ((start (point))
            (face (pcase (scv-message-role msg)
                    ("user" 'scv-viewer-user)
                    ("assistant" 'scv-viewer-assist)
                    (_ 'scv-viewer-footer))))
        (insert (format ">>> %s\n" (scv-message-role msg)))
        (insert (scv-message-content msg))
        (insert "\n\n")
        (put-text-property start (point) 'face face)))
    (let ((tools (scv-projection-tools proj)))
      (when tools
        (insert (propertize "--- tools ---\n" 'face 'scv-viewer-footer))
        (dolist (tool tools)
          (let ((start (point))
                (face (pcase (scv-tool-status tool)
                        ("running" 'scv-viewer-tool-run)
                        ("success" 'scv-viewer-tool-ok)
                        ("failure" 'scv-viewer-tool-err)
                        (_ 'scv-viewer-footer))))
            (insert (format "[%s] %s %s"
                            (pcase (scv-tool-status tool)
                              ("running" "…")
                              ("success" "✓")
                              ("failure" "✗")
                              (_ "?"))
                            (scv-tool-name tool)
                            (scv-tool-input-preview tool)))
            (when (and (not (string-empty-p (scv-tool-result tool)))
                       (member (scv-tool-status tool) '("success" "failure")))
              (insert (format " → %s"
                              (truncate-string-to-width
                               (scv-tool-result tool) 80 nil nil "…"))))
            (insert "\n")
            (put-text-property start (point) 'face face)))
        (insert "\n")))
    ;; Handoff progress
    (let ((handoffs (scv-projection-handoff-progress proj)))
      (when handoffs
        (insert (propertize "--- handoff ---\n" 'face 'scv-viewer-footer))
        (dolist (h handoffs)
          (let ((start (point)))
            (insert (format "  [%s] %s\n"
                            (scv-handoff-stage h)
                            (truncate-string-to-width
                             (scv-handoff-text h) 80 nil nil "…")))
            (put-text-property start (point) 'face 'scv-viewer-blocker)))
        (insert "\n")))
    (let ((start (point)))
      (insert (scv-session-viewer--render-footer-text proj))
      (put-text-property start (point) 'face
                         (scv-session-viewer--state-face
                          (scv-projection-state proj))))
    (let ((start (point)))
      (insert (scv-session-viewer--render-input-area-text))
      (put-text-property start (point) 'face 'scv-viewer-prompt))
    (goto-char (point-min))))

;;; Buffer-local state

(defvar-local scv-session-viewer--projection nil
  "Current projection in this buffer.")

(defvar-local scv-session-viewer--session-id nil
  "Session id for this viewer.")

(defvar-local scv-session-viewer--byte-pos 0
  "Last byte position read from the JSONL log.")

(defvar-local scv-session-viewer--poll-timer nil
  "Polling timer for JSONL log.")

(defvar-local scv-session-viewer--socket-process nil
  "Socket process for live updates.")

(defvar-local scv-session-viewer--last-state nil
  "Last known projection state for status messaging.")

(defvar-local scv-session-viewer--running-start-time nil
  "Timestamp when state entered running.")

(defvar-local scv-session-viewer--running-quota-alerted nil
  "Whether long-running work alert has been shown.")

(defun scv-session-viewer--announce-state (state)
  "Emit a SCV quote when entering STATE for long-running work."
  (cond
   ((eq state 'running)
    (if (not (eq scv-session-viewer--last-state 'running))
        (progn
          (setq scv-session-viewer--running-start-time (float-time))
          (setq scv-session-viewer--running-quota-alerted nil)
          (message "In the rear with the gear!"))
      (when (and (not scv-session-viewer--running-quota-alerted)
                 scv-session-viewer--running-start-time
                 (> (- (float-time) scv-session-viewer--running-start-time) 120))
        (setq scv-session-viewer--running-quota-alerted t)
        (message "%s"
                 (if (fboundp 'scv--annoyed-quote)
                     (scv--annoyed-quote 'cpu-stress)
                   "Heck! Over-worked, under-paid, and under-armored!")))))
   (t
    (setq scv-session-viewer--running-start-time nil)
    (setq scv-session-viewer--running-quota-alerted nil)))
  (setq scv-session-viewer--last-state state))

;;; Event processing

;;; Auto-scroll

(defvar-local scv-session-viewer--user-at-bottom t
  "Non-nil when the user's point is near the buffer end.

When true, new content auto-scrolls to follow output.
When false (user scrolled up to read history), we stay put.")

(defun scv-session-viewer--scroll-to-bottom ()
  "Scroll all windows showing this buffer to the bottom."
  (when scv-session-viewer--user-at-bottom
    (dolist (win (get-buffer-window-list (current-buffer) nil t))
      (when (window-live-p win)
        (with-selected-window win
          (goto-char (point-max))
          (recenter -1))))))

(defun scv-session-viewer--process-events (events)
  "Process list of EVENTS and update the buffer."
  (when events
	   (let* ((old (or scv-session-viewer--projection
                    (scv-reducer--init-state
                     scv-session-viewer--session-id)))
             (new (scv-reducer-apply-events
                  events (scv-projection-session-id old))))
	      (setq scv-session-viewer--projection new)
	      (scv-session-viewer--announce-state (scv-projection-state new))
	      (let ((inhibit-read-only t))
	        (scv-session-viewer--render-full new))
	      (scv-session-viewer--scroll-to-bottom))))

(defun scv-session-viewer--process-event (event)
  "Process single EVENT and update the buffer incrementally."
  (let* ((old (or scv-session-viewer--projection
                  (scv-reducer--init-state
                   scv-session-viewer--session-id)))
         (new (scv-reducer--apply-one-event old event)))
    (setq scv-session-viewer--projection new)
    (scv-session-viewer--announce-state (scv-projection-state new))
    (let ((inhibit-read-only t))
      (scv-session-viewer--erase-footer)
      (goto-char (point-max))
      (let ((old-msg-len (length (scv-projection-messages old)))
            (new-msg-len (length (scv-projection-messages new)))
            (old-tools (scv-projection-tools old))
            (new-tools (scv-projection-tools new)))
        ;; New messages
        (when (> new-msg-len old-msg-len)
          (dolist (msg (seq-drop (scv-projection-messages new) old-msg-len))
            (let ((start (point))
                  (face (pcase (scv-message-role msg)
                          ("user" 'scv-viewer-user)
                          ("assistant" 'scv-viewer-assist)
                          (_ 'scv-viewer-footer))))
              (insert (format "\n>>> %s\n" (scv-message-role msg)))
              (insert (scv-message-content msg))
              (insert "\n")
              (put-text-property start (point) 'face face))))
        ;; Tools section: always re-render from scratch to catch status changes
        ;; Erase any existing tools section
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^--- tools ---\n" nil t)
            (delete-region (match-beginning 0) (point-max))))
        ;; Re-render tools
        (when new-tools
          (insert "\n--- tools ---\n")
          (dolist (tool new-tools)
            (let ((start (point))
                  (face (pcase (scv-tool-status tool)
                          ("running" 'scv-viewer-tool-run)
                          ("success" 'scv-viewer-tool-ok)
                          ("failure" 'scv-viewer-tool-err)
                          (_ 'scv-viewer-footer))))
              (insert (format "[%s] %s %s"
                              (pcase (scv-tool-status tool)
                                ("running" "…")
                                ("success" "✓")
                                ("failure" "✗")
                                (_ "?"))
                              (scv-tool-name tool)
                              (scv-tool-input-preview tool)))
              (when (and (not (string-empty-p (scv-tool-result tool)))
                         (member (scv-tool-status tool) '("success" "failure")))
                (insert (format " → %s"
                                (truncate-string-to-width
                                 (scv-tool-result tool) 80 nil nil "…"))))
              (insert "\n")
              (put-text-property start (point) 'face face)))
          (insert "\n"))
        ;; Handoff section: always re-render from scratch
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^--- handoff ---\n" nil t)
            (delete-region (match-beginning 0) (point-max))))
        (let ((handoffs (scv-projection-handoff-progress new)))
          (when handoffs
            (insert "--- handoff ---\n")
            (dolist (h handoffs)
              (insert (format "  [%s] %s\n"
                              (scv-handoff-stage h)
                              (truncate-string-to-width
                               (scv-handoff-text h) 80 nil nil "…"))))
            (insert "\n")))
        ;; Footer + input area
        (let ((start (point)))
          (insert (scv-session-viewer--render-footer-text new))
          (put-text-property start (point) 'face
                             (scv-session-viewer--state-face
                              (scv-projection-state new))))
        (let ((start (point)))
          (insert (scv-session-viewer--render-input-area-text))
          (put-text-property start (point) 'face 'scv-viewer-prompt))
        (scv-session-viewer--scroll-to-bottom)))))

;;; Polling

(defun scv-session-viewer--poll-callback ()
  "Timer callback: check for new JSONL events."
  (when (buffer-live-p (current-buffer))
    (let* ((log (scv-session-viewer--log-path
                 scv-session-viewer--session-id))
           (res (scv-session-viewer--read-new-events
                 log scv-session-viewer--byte-pos))
           (evs (car res))
           (pos (cdr res)))
      (when evs
        (scv-session-viewer--process-events evs)
        (setq scv-session-viewer--byte-pos pos)))))

;;; Socket transport

(defun scv-session-viewer--socket-sentinel (proc event)
  "Handle socket PROC EVENT."
  (when (buffer-live-p (current-buffer))
    (when (memq (process-status proc) '(exit signal closed))
      (setq scv-session-viewer--socket-process nil)
      (scv-session-viewer--start-polling
       scv-session-viewer--session-id))))

(defun scv-session-viewer--alist-get-string (key alist)
  "Return KEY from ALIST with symbol-or-string compatibility."
  (or (alist-get key alist)
      (alist-get (if (symbolp key) (symbol-name key) key) alist nil nil #'string=)))

(defun scv-session-viewer--string-list (value)
  "Convert VALUE into a list of non-empty strings."
  (seq-filter
   #'identity
   (mapcar
    (lambda (item)
      (and (stringp item)
           (let ((trimmed (string-trim item)))
             (if (string-empty-p trimmed) nil trimmed))))
    (if (listp value) value '()))))

(defun scv-session-viewer--permission-option-pairs (value)
  "Convert permission option VALUE into (LABEL . DECISION) pairs."
  (or
   (seq-filter
    #'identity
    (mapcar
     (lambda (item)
       (cond
        ((and (consp item)
              (stringp (scv-session-viewer--alist-get-string 'label item))
              (stringp (scv-session-viewer--alist-get-string 'value item)))
         (cons (scv-session-viewer--alist-get-string 'label item)
               (scv-session-viewer--alist-get-string 'value item)))
        ((stringp item) (cons item item))
        (t nil)))
     (if (listp value) value '())))
   '(("Allow once" . "allow_once")
     ("Deny" . "reject_once")
     ("Allow always" . "allow_always")
     ("Allow always globally" . "allow_always_global"))))

(defun scv-session-viewer--make-action-process (args)
  "Run scv action ARGS with SCV integration environment."
  (let* ((command (apply #'scv--command args))
         (buffer (get-buffer-create "*scv action*")))
    (let ((process-environment (scv--integration-environment)))
      (make-process
       :name (format "scv-action-%s" (or scv-session-viewer--session-id "unknown"))
       :buffer buffer
       :command command
       :connection-type 'pipe
       :noquery t))))

(defun scv-session-viewer--run-session-action (args)
  "Run a session action command with ARGS."
  (ignore (scv-session-viewer--make-action-process args))
  (message "Orders received."))

(defun scv-session-viewer--selector-prompt (label options current)
  "Ask for a selector value from OPTIONS with LABEL and CURRENT."
  (let ((choice
         (completing-read (format "%s (current: %s): " label (or current ""))
                          options nil t nil nil (or current ""))))
    (unless (string-empty-p choice) choice)))

(defun scv-session-viewer--selector-options-prompt (selector options current)
  (let* ((label (format "SCV %s" selector))
         (choice (scv-session-viewer--selector-prompt label options current)))
    (and (stringp choice) (string-trim choice))))

(defun scv-session-viewer--selector-request (frame)
  "Handle a selector request FRAME from the host."
  (let* ((selector (scv-session-viewer--alist-get-string 'selector frame))
         (current (or (scv-session-viewer--alist-get-string 'current frame) ""))
         (options (scv-session-viewer--string-list
                   (scv-session-viewer--alist-get-string 'options frame)))
         (selection (scv-session-viewer--selector-options-prompt selector options current))
         (session-id scv-session-viewer--session-id))
    (when (and selection (not (string-empty-p selection)))
      (pcase selector
        ("model"
         (scv-session-viewer--run-session-action
          (append scv-session-action-set-model-command
                  (list "--session-id" session-id selection))))
        ("reasoning"
         (scv-session-viewer--run-session-action
          (append scv-session-action-set-reasoning-command
                  (list "--session-id" session-id selection))))
        ("skill"
         (scv-session-viewer--run-session-action
          (append scv-session-action-set-skills-command
                  (append (list "--session-id" session-id) (list selection)))))
        ("permission"
         (scv-session-viewer--run-session-action
          (append scv-session-action-set-permission-command
                  (list "--session-id" session-id selection))))
        (_ (message "Unknown selector: %s" selector))))))

(defun scv-session-viewer--permission-request (frame)
  "Handle a permission request FRAME from the host."
  (let* ((request-id (scv-session-viewer--alist-get-string 'request_id frame))
         (tool-name (or (scv-session-viewer--alist-get-string 'tool_name frame) "tool"))
         (input-preview (or (scv-session-viewer--alist-get-string 'input_preview frame)
                            ""))
         (options (scv-session-viewer--permission-option-pairs
                   (scv-session-viewer--alist-get-string 'options frame)))
         (session-id scv-session-viewer--session-id)
         (label (format "Permission for %s (%s)" tool-name
                        (truncate-string-to-width input-preview 40)))
         (selection-label (completing-read
                          (format "%s: " label)
                          (mapcar #'car options)
                          nil t nil nil nil))
         (selection (alist-get selection-label options nil nil #'string=)))
    (if selection
        (scv-session-viewer--run-session-action
         (append scv-session-action-permission-command
                 (list "--session-id" session-id request-id selection)))
      (message "No permission decision selected."))))

(defun scv-session-viewer--socket-filter (proc string)
  "Process STRING from socket PROC."
  (when (buffer-live-p (current-buffer))
    (let ((accum (process-get proc :accum))
          (lines (split-string string "\n")))
      (setf (process-get proc :accum) (concat accum (car lines)))
      (dolist (line (cdr lines))
        (let ((full (concat (process-get proc :accum) line)))
          (setf (process-get proc :accum) "")
            (let ((frame (scv-session-viewer--parse-line full)))
              (when frame
              (let ((type (scv-session-viewer--alist-get-string 'type frame)))
                (pcase type
                  ("catch_up_start"
                   (setq scv-session-viewer--projection nil))
                  ("event"
                   (let ((ev (scv-session-viewer--alist-get-string 'event frame)))
                     (when ev
                       (scv-session-viewer--process-event ev))))
                  ("selector_request" (scv-session-viewer--selector-request frame))
                  ("permission_request"
                   (scv-session-viewer--permission-request frame))
                  ("catch_up_end" nil)
                  (_ nil))))))))))

(defun scv-session-viewer--connect-socket (session-id)
  "Connect to Session Host socket. Returns t on success."
  (let ((sock (scv-session-viewer--socket-path session-id)))
    (if (not (file-exists-p sock))
        nil
      (condition-case nil
          (let ((proc (make-network-process
                       :name (format "scv-host-%s" session-id)
                       :family 'local
                       :service sock
                       :nowait t
                       :coding 'utf-8-emacs-unix
                       :filter #'scv-session-viewer--socket-filter
                       :sentinel #'scv-session-viewer--socket-sentinel)))
            (setq scv-session-viewer--socket-process proc)
            (when scv-session-viewer--poll-timer
              (cancel-timer scv-session-viewer--poll-timer)
              (setq scv-session-viewer--poll-timer nil))
            t)
        (file-error nil)
        (error nil)))))

(defun scv-session-viewer--start-polling (session-id)
  "Start JSONL polling for SESSION-ID."
  (when scv-session-viewer--poll-timer
    (cancel-timer scv-session-viewer--poll-timer))
  (setq scv-session-viewer--poll-timer
        (run-with-timer 0.1 0.1
                        #'scv-session-viewer--poll-callback)))

;;; Prompt integration

(defun scv-session-viewer--on-prompt-submit ()
  "Called when user submits a prompt — opens the Rich Prompt Box."
  (interactive)
  (let ((session-id scv-session-viewer--session-id))
    (when session-id
      (scv-open-prompt-draft session-id))))

(defun scv-session-viewer--type-to-prompt (&optional n)
  "Redirect self-insert to the prompt draft, inserting the typed character."
  (interactive "p")
  (let* ((session-id scv-session-viewer--session-id)
         (char (when (characterp last-command-event) last-command-event)))
    (if (and session-id char)
        (progn
          (scv-open-prompt-draft session-id)
          (dotimes (_ (or n 1))
            (insert-char char)))
      (when session-id
        (scv-open-prompt-draft session-id)))))

;;; Interrupt

(defun scv-session-viewer--interrupt ()
  "Interrupt the current Coding Session."
  (interactive)
  (let ((session-id scv-session-viewer--session-id))
    (unless session-id
      (user-error "No session id bound to this viewer"))
    (scv-session-viewer--run-session-action
     (append scv-session-action-interrupt-command
             (list "--session-id" session-id)))
    (message "Hey! You're stressin' me out here!")))

;;; Navigation

(defun scv-session-viewer-scroll-up ()
  "Scroll toward newer output and resume following output."
  (interactive)
  (scroll-up-command)
  (setq-local scv-session-viewer--user-at-bottom t))

(defun scv-session-viewer-scroll-down ()
  "Scroll toward older output and pause following when away from bottom."
  (interactive)
  (scroll-down-command)
  (when (< (point) (- (point-max) 300))
    (setq-local scv-session-viewer--user-at-bottom nil)))

(defun scv-session-viewer-beginning ()
  "Jump to the start of the Session Viewer."
  (interactive)
  (goto-char (point-min))
  (setq-local scv-session-viewer--user-at-bottom nil))

(defun scv-session-viewer-end ()
  "Jump to the live bottom of the Session Viewer."
  (interactive)
  (goto-char (point-max))
  (setq-local scv-session-viewer--user-at-bottom t)
  (scv-session-viewer--scroll-to-bottom))

;;; Public API

;;;###autoload
(defun scv-session-viewer-open (session-id)
  "Open native Emacs Session Viewer for SESSION-ID."
  (interactive (list (read-string "Session id: ")))
  (let* ((origin scv-default-origin)
         (key (scv--session-key origin session-id))
         (buf (get-buffer-create
               (format "*scv %s*" session-id))))
    (with-current-buffer buf
      (fundamental-mode)
      (buffer-disable-undo)
      (setq-local scv-session-viewer--session-id session-id)
      (setq-local scv-session-viewer--projection nil)
      (setq-local scv-session-viewer--last-state nil)
      (setq-local scv-session-viewer--byte-pos 0)
      (setq-local scv-session-id session-id)
      (setq-local scv-origin origin)
      (setq-local scv-session-viewer--user-at-bottom t)
      (use-local-map
       (let ((map (make-sparse-keymap)))
         (define-key map (kbd "RET")
                     #'scv-session-viewer--on-prompt-submit)
         (define-key map (kbd "C-c C-c")
                     #'scv-session-viewer--on-prompt-submit)
         (define-key map [remap self-insert-command]
                     #'scv-session-viewer--type-to-prompt)
         (define-key map (kbd "i") #'scv-session-viewer--interrupt)
         (define-key map (kbd "n") #'scv-session-viewer-scroll-up)
         (define-key map (kbd "P") #'scv-session-viewer-scroll-down)
         (define-key map [mouse-5] #'scv-session-viewer-scroll-up)
         (define-key map [mouse-4] #'scv-session-viewer-scroll-down)
         (define-key map (kbd "<next>") #'scv-session-viewer-scroll-up)
         (define-key map (kbd "<prior>") #'scv-session-viewer-scroll-down)
         (define-key map (kbd "g") #'scv-session-viewer-beginning)
         (define-key map (kbd "G") #'scv-session-viewer-end)
         (define-key map (kbd "/") #'isearch-forward)
         map))
      (setq-local buffer-read-only t)
      (puthash key buf scv--viewer-buffers)
      ;; Initial catch-up from log
      (let* ((log (scv-session-viewer--log-path session-id))
             (res (scv-session-viewer--read-all-events log)))
        (setq scv-session-viewer--byte-pos (cdr res))
        (let ((proj (if (car res)
                        (scv-reducer-apply-events (car res) session-id)
                      (scv-reducer--init-state session-id))))
          (setq scv-session-viewer--projection proj)
          (scv-session-viewer--render-full proj)
          (scv-session-viewer--announce-state
           (scv-projection-state proj))))
      ;; Socket or poll
      (unless (scv-session-viewer--connect-socket session-id)
        (scv-session-viewer--start-polling session-id)))
    (pop-to-buffer buf)))

;;;###autoload
(defun scv-session-viewer-close ()
  "Close current Session Viewer."
  (interactive)
  (when scv-session-viewer--poll-timer
    (cancel-timer scv-session-viewer--poll-timer)
    (setq scv-session-viewer--poll-timer nil))
  (when scv-session-viewer--socket-process
    (ignore-errors
      (delete-process scv-session-viewer--socket-process))
    (setq scv-session-viewer--socket-process nil))
  (kill-buffer (current-buffer)))

(provide 'scv-session-viewer)
;;; scv-session-viewer.el ends here
