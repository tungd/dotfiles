;;; scv-reducer.el --- Pure Elisp event-to-projection reducer -*- lexical-binding: t; -*-

;; Replay a list of decoded JSONL events into a structured projection.
;; This is the Elisp equivalent of OCaml's session_client_projection.ml.
;; Pure function: events → projection, trivially testable.
;;
;; Package-Requires: ((emacs "27.1") (cl-lib "1.0"))

;;; Code:

(require 'cl-lib)

;;; Types

(cl-defstruct scv-message
  index
  role       ; "user" | "assistant"
  event-type ; "prompt_submit" | "provider_response" | etc.
  status     ; "pending" | "complete" | "streaming" | "interrupted"
  content)

(cl-defstruct scv-tool
  index
  name
  input
  input-preview
  result
  status       ; "running" | "success" | "failure" | "skipped" | "cancelled"
  error
  started-at
  completed-at)

(cl-defstruct scv-blocker
  id           ; request_id or question_id
  type         ; permission | question
  tool-name    ; nil for questions
  detail       ; input_preview or question text
  status       ; "pending" | "allowed" | "rejected"
  decision)    ; decision string when resolved

(cl-defstruct scv-queue
  (steering ()))

(cl-defstruct scv-handoff
  index
  event-type
  stage
  attempt-id
  text)

(cl-defstruct scv-projection
  session-id
  state                ; created | idle | running | prompt_queued |
                       ; blocked_on_permission | blocked_on_question |
                       ; handing_off | interrupted | completed | failed
  messages             ; list of scv-message
  tools                ; list of scv-tool
  blockers             ; list of scv-blocker
  queue                ; scv-queue
  handoff-progress     ; list of scv-handoff
  model
  provider
  reasoning-level
  active-skills        ; list of string
  cwd
  task-title
  task-title-source
  permission-mode      ; ask | auto | plan
  backend-connected)

;;; Helper

(defun scv-reducer--str (val &optional fallback)
  "Return VAL as string, or FALLBACK when nil."
  (cond ((stringp val) val)
        ((numberp val) (number-to-string val))
        ((eq val t) "true")
        ((eq val :json-false) "false")
        ((null val) (or fallback ""))
        (t (format "%s" val))))

(defun scv-reducer--alist-get (key alist &optional fallback)
  "Get KEY from ALIST (decoded JSON), accepting symbol or string keys."
  (let ((name (if (symbolp key) (symbol-name key) key)))
    (or (alist-get key alist)
        (and (stringp name)
             (alist-get name alist nil nil #'string=))
        (and (stringp key)
             (alist-get (intern key) alist))
        fallback)))

(defun scv-reducer--json-pair-value (pair)
  "Return the JSON value from a ppx_yojson field PAIR."
  (cond
   ((and (consp pair) (consp (cdr pair)) (null (cddr pair)))
    (cadr pair))
   ((consp pair) (cdr pair))
   (t nil)))

(defun scv-reducer--json-fields-get (key fields &optional fallback)
  "Get KEY from ppx_yojson FIELDS, accepting symbol or string keys."
  (let ((name (if (symbolp key) (symbol-name key) key))
        (found nil)
        value)
    (dolist (pair fields)
      (let ((pair-name
             (cond
              ((and (consp pair) (symbolp (car pair))) (symbol-name (car pair)))
              ((and (consp pair) (stringp (car pair))) (car pair))
              (t nil))))
        (when (and (not found)
                   (stringp name)
                   pair-name
                   (string= pair-name name))
          (setq found t)
          (setq value (scv-reducer--json-pair-value pair)))))
    (if found value fallback)))

(defun scv-reducer--payload (event)
  "Return EVENT's typed ppx_yojson payload."
  (let ((payload (scv-reducer--alist-get 'event event)))
    (and (consp payload)
         (stringp (car payload))
         payload)))

(defun scv-reducer--payload-fields (event)
  "Return EVENT's typed ppx_yojson payload fields."
  (let ((payload (scv-reducer--payload event)))
    (if (and payload (listp (cadr payload)))
        (cadr payload)
      '())))

(defun scv-reducer--variant-name->event-type (name)
  "Convert ppx_yojson variant NAME to the reducer event type string."
  (downcase name))

(defun scv-reducer--find-tool-by-index (tools tool-call-id)
  "Find a tool in TOOLS by its tool_call_id stored in index metadata."
  (cl-find-if (lambda (tool)
                (and (stringp (scv-tool-index tool))
                     (string= (scv-tool-index tool) tool-call-id)))
              (reverse tools)))

(defun scv-reducer--find-tool-by-tool-name (tools name)
  "Find the most recent running tool in TOOLS matching NAME."
  (cl-find-if (lambda (tool)
                (and (string= (scv-tool-name tool) name)
                     (string= (scv-tool-status tool) "running")))
              (reverse tools)))

;;; Field extractors

(defun scv-reducer--event-type (event)
  "Return the event type string from a decoded event ALIST."
  (let ((payload (scv-reducer--payload event)))
    (if payload
        (scv-reducer--variant-name->event-type (car payload))
      "unknown")))

(defun scv-reducer--field (event key &optional fallback)
  "Return field KEY from EVENT's typed JSON fields."
  (let ((marker (make-symbol "missing")))
    (let ((payload-value
           (scv-reducer--json-fields-get
            key (scv-reducer--payload-fields event) marker)))
      (if (eq payload-value marker)
          (scv-reducer--alist-get key event fallback)
        payload-value))))

;;; Core reducer

(defun scv-reducer--init-state (&optional session-id)
  "Return an empty projection."
  (make-scv-projection
   :session-id session-id
   :state 'created
   :messages ()
   :tools ()
   :blockers ()
   :queue (make-scv-queue)
   :handoff-progress ()
   :backend-connected t))

(defun scv-reducer--apply-one-event (proj event)
  "Apply one decoded EVENT to projection PROJ, returning a new projection."
  (let* ((etype (scv-reducer--event-type event))
         (index (scv-reducer--field event 'index))
         (session-id (or (scv-reducer--field event 'session_id)
                         (scv-projection-session-id proj))))
    (cl-case (intern etype)
      (session_start
       (let ((p (copy-scv-projection proj)))
         (setf (scv-projection-session-id p) session-id
               (scv-projection-model p) (scv-reducer--field event 'model)
               (scv-projection-provider p) (scv-reducer--field event 'provider)
               (scv-projection-cwd p) (scv-reducer--field event 'cwd)
               (scv-projection-task-title p) (scv-reducer--field event 'task_title)
               (scv-projection-task-title-source p) (scv-reducer--field event 'task_title_source)
               (scv-projection-permission-mode p) 'ask
               (scv-projection-state p) 'idle)
         p))

      (prompt_submit
       (let* ((p (copy-scv-projection proj))
              (msg (make-scv-message
                    :index (or index (length (scv-projection-messages p)))
                    :role "user"
                    :event-type "prompt_submit"
                    :status "complete"
                    :content (scv-reducer--field event 'query ""))))
         (setf (scv-projection-state p) 'running
               (scv-projection-messages p) (nconc (scv-projection-messages p) (list msg)))
         p))

      (provider_response
       (let* ((p (copy-scv-projection proj))
              (resp (or (scv-reducer--field event 'response) ""))
              (msg (make-scv-message
                    :index (or index (length (scv-projection-messages p)))
                    :role "assistant"
                    :event-type "provider_response"
                    :status "complete"
                    :content (scv-reducer--str resp))))
         (setf (scv-projection-messages p) (nconc (scv-projection-messages p) (list msg))
               (scv-projection-state p) (if (string= (scv-reducer--field event 'status "") "completed")
                                             'completed 'idle))
         p))

      (provider_response_complete
       (let ((p (copy-scv-projection proj)))
         (setf (scv-projection-state p)
               (if (string= (scv-reducer--field event 'status "completed") "completed")
                   'completed 'idle))
         p))

      (assistant_delta
       ;; Streaming text appended to the last assistant message
       (let* ((p (copy-scv-projection proj))
              (msgs (scv-projection-messages p))
              (last (car (last msgs)))
              (delta (scv-reducer--field event 'response "")))
         (when (and last (string= (scv-message-role last) "assistant"))
           (setf (scv-message-content last)
                 (concat (scv-message-content last) (scv-reducer--str delta)))
           (setf (scv-projection-state p) 'running))
         p))

      (reasoning_delta
       ;; Append reasoning text to the last assistant message's reasoning field
       (let* ((p (copy-scv-projection proj))
              (msgs (scv-projection-messages p))
              (last (car (last msgs)))
              (delta (scv-reducer--field event 'response "")))
         (when (and last (string= (scv-message-role last) "assistant"))
           (setf (scv-projection-state p) 'running))
         ;; For phase 1, we store reasoning in a separate tracking field.
         ;; Phase 2: render reasoning as dimmed blocks.
         p))

      ((tool_call tool_start)
       ;; A new tool call starts
       (let* ((p (copy-scv-projection proj))
              (tool-name (scv-reducer--field event 'tool_name "tool"))
              (tool-input (scv-reducer--field event 'tool_input))
              (input-preview (scv-reducer--field event 'input_preview))
              (tool-call-id (scv-reducer--field event 'tool_call_id))
              (input-str
               (cond ((stringp tool-input) tool-input)
                     ((and (listp tool-input) (alist-get 'command tool-input))
                      (scv-reducer--str (alist-get 'command tool-input)))
                     (scv-reducer--field event 'command)
                     ((listp tool-input) (format "%s" tool-input))
                     (t "")))
              (tool (make-scv-tool
                     :index (or tool-call-id (format "tool-%d" (length (scv-projection-tools p))))
                     :name tool-name
                     :input (scv-reducer--str input-str)
                     :input-preview (scv-reducer--str (or input-preview input-str))
                     :result ""
                     :status "running"
                     :error nil
                     :started-at (scv-reducer--field event 'timestamp))))
         (setf (scv-projection-tools p) (nconc (scv-projection-tools p) (list tool))
               (scv-projection-state p) 'running)
         p))

      ((tool_complete tool_result)
       ;; Update the matching tool with result and status
       (let* ((p (copy-scv-projection proj))
              (tools (copy-sequence (scv-projection-tools p)))
              (tool-call-id (scv-reducer--field event 'tool_call_id))
              (tool-name (scv-reducer--field event 'tool_name))
              (resp (scv-reducer--field event 'result
                       (scv-reducer--field event 'response "")))
              (err (scv-reducer--field event 'error))
              (status-str (scv-reducer--field event 'status)))
         ;; Try to find by tool_call_id first, then by name
         (let ((tool (or (scv-reducer--find-tool-by-index tools tool-call-id)
                         (scv-reducer--find-tool-by-tool-name tools tool-name))))
           (when tool
             (let ((new-tool (copy-scv-tool tool)))
               (setf (scv-tool-result new-tool) (scv-reducer--str resp)
                     (scv-tool-status new-tool) (if (and status-str (not (string-empty-p status-str)))
                                                     status-str
                                                   (if (and err (not (string-empty-p (scv-reducer--str err))))
                                                       "failure" "success"))
                     (scv-tool-error new-tool) (scv-reducer--str err "")
                     (scv-tool-completed-at new-tool) (scv-reducer--field event 'timestamp))
               (setf (scv-projection-tools p)
                     (nconc (cl-remove tool tools :test #'eq) (list new-tool))))))
         p))

      (tool_progress
       ;; Update the matching tool with a live progress message
       (let* ((p (copy-scv-projection proj))
              (tools (copy-sequence (scv-projection-tools p)))
              (tool-call-id (scv-reducer--field event 'tool_call_id))
              (tool-name (scv-reducer--field event 'tool_name))
              (message (scv-reducer--field event 'message "")))
         (when (and message (not (string-empty-p message)))
           (let ((tool (or (scv-reducer--find-tool-by-index tools tool-call-id)
                           (scv-reducer--find-tool-by-tool-name tools tool-name))))
             (when tool
               (let ((new-tool (copy-scv-tool tool)))
                 (setf (scv-tool-result new-tool) (scv-reducer--str message))
                 (setf (scv-projection-tools p)
                       (nconc (cl-remove tool tools :test #'eq) (list new-tool)))))))
         p))

      ((permission_requested permission_request)
       ;; A permission blocker appears
       (let* ((p (copy-scv-projection proj))
              (request-id (scv-reducer--field event 'request_id))
              (blocker (make-scv-blocker
                        :id request-id
                        :type 'permission
                        :tool-name (scv-reducer--field event 'tool_name)
                        :detail (scv-reducer--field event 'input_preview
                                                        (scv-reducer--field event 'reason ""))
                        :status (scv-reducer--field event 'status "pending"))))
         (setf (scv-projection-state p) 'blocked_on_permission
               (scv-projection-blockers p) (nconc (scv-projection-blockers p) (list blocker)))
         p))

      ((permission_resolved permission_replied)
       ;; A permission blocker is resolved
       (let* ((p (copy-scv-projection proj))
              (request-id (scv-reducer--field event 'request_id))
              (decision (scv-reducer--field event 'decision))
              (status (scv-reducer--field event 'status))
              (blockers (scv-projection-blockers p))
              (blocker (cl-find-if (lambda (b) (string= (scv-blocker-id b) request-id)) blockers)))
         (when blocker
           (let ((new (copy-scv-blocker blocker)))
             (setf (scv-blocker-status new) (or status "resolved")
                   (scv-blocker-decision new) decision)
             (setf (scv-projection-blockers p)
                   (nconc (cl-remove blocker blockers :test #'eq) (list new)))))
         ;; If no more pending blockers, transition to idle
         (when (cl-notany (lambda (b) (string= (scv-blocker-status b) "pending"))
                          (scv-projection-blockers p))
           (setf (scv-projection-state p) 'idle))
         p))

      (question_asked
       ;; A question blocker appears
       (let* ((p (copy-scv-projection proj))
              (question-id (scv-reducer--field event 'question_id))
              (blocker (make-scv-blocker
                        :id question-id
                        :type 'question
                        :tool-name nil
                        :detail (scv-reducer--field event 'question "")
                        :status (scv-reducer--field event 'status "pending"))))
         (setf (scv-projection-state p) 'blocked_on_question
               (scv-projection-blockers p) (nconc (scv-projection-blockers p) (list blocker)))
         p))

      (question_answered
       ;; A question blocker is resolved
       (let* ((p (copy-scv-projection proj))
              (question-id (scv-reducer--field event 'question_id))
              (blockers (scv-projection-blockers p))
              (blocker (cl-find-if (lambda (b) (string= (scv-blocker-id b) question-id)) blockers)))
         (when blocker
           (let ((new (copy-scv-blocker blocker)))
             (setf (scv-blocker-status new) "answered")
             (setf (scv-projection-blockers p)
                   (nconc (cl-remove blocker blockers :test #'eq) (list new)))))
         (when (cl-notany (lambda (b) (string= (scv-blocker-status b) "pending"))
                          (scv-projection-blockers p))
           (setf (scv-projection-state p) 'idle))
         p))

      (set_permission_mode
       (let ((p (copy-scv-projection proj))
             (mode (scv-reducer--field event 'mode)))
         (setf (scv-projection-permission-mode p) (intern mode))
         p))

      (set_model
       (let ((p (copy-scv-projection proj)))
         (setf (scv-projection-model p) (scv-reducer--field event 'model))
         p))

      (set_reasoning_level
       (let ((p (copy-scv-projection proj)))
         (setf (scv-projection-reasoning-level p) (scv-reducer--field event 'reasoning_level))
         p))

      (set_skills
       (let ((p (copy-scv-projection proj))
             (skills (scv-reducer--field event 'skills)))
         (when (listp skills)
           (setf (scv-projection-active-skills p)
                 (mapcar (lambda (s) (if (stringp s) s (format "%s" s))) skills)))
         p))

      (interrupted
       (let ((p (copy-scv-projection proj)))
         (setf (scv-projection-state p) 'interrupted)
         p))

      (session_error
       (let ((p (copy-scv-projection proj)))
         (setf (scv-projection-state p) 'failed)
         p))

      (stop
       (let ((p (copy-scv-projection proj)))
         (setf (scv-projection-state p) 'completed)
         p))

      (idle_prompt
       (let ((p (copy-scv-projection proj)))
         (setf (scv-projection-state p) 'idle)
         p))

      (backend_disconnected
       (let ((p (copy-scv-projection proj)))
         (setf (scv-projection-state p) 'failed)
         p))

      (clear_queue
       (let ((p (copy-scv-projection proj)))
         (setf (scv-projection-queue p) (make-scv-queue))
         p))

      (handoff_progress
       (let* ((p (copy-scv-projection proj))
              (h (make-scv-handoff
                  :index (length (scv-projection-handoff-progress p))
                  :event-type "handoff_progress"
                  :stage (scv-reducer--field event 'stage)
                  :attempt-id (scv-reducer--field event 'attempt_id)
                  :text (scv-reducer--field event 'text
                           (scv-reducer--field event 'reason "")))))
         (setf (scv-projection-state p) 'handing_off
               (scv-projection-handoff-progress p)
               (nconc (scv-projection-handoff-progress p) (list h)))
         p))

      (waiting_for_input
       (let ((p (copy-scv-projection proj)))
         (setf (scv-projection-state p) 'blocked_on_question)
         p))

      (session_title
       (let ((p (copy-scv-projection proj)))
         (setf (scv-projection-task-title p) (scv-reducer--field event 'task_title)
               (scv-projection-task-title-source p) (scv-reducer--field event 'task_title_source "explicit"))
         p))

      (state_changed
       (let ((p (copy-scv-projection proj))
             (new-state (scv-reducer--field event 'state)))
         (when new-state
           (setf (scv-projection-state p) (intern new-state)))
         p))

      (t proj)))) ;; Unknown event type: pass through

(defun scv-reducer-apply-events (events &optional session-id)
  "Apply a list of decoded JSON EVENTs to a projection.

EVENTS is a list of alists (decoded JSON objects).
Returns a scv-projection struct.
SESSION-ID is used when the first event lacks one."
  (let ((proj (scv-reducer--init-state session-id)))
    (dolist (event events proj)
      (setq proj (scv-reducer--apply-one-event proj event)))))

(provide 'scv-reducer)
;;; scv-reducer.el ends here
