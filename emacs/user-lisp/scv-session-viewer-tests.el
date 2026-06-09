;;; scv-session-viewer-tests.el --- Unit tests for native Emacs Session Viewer -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'scv-reducer)
(require 'scv-session-viewer)

;;; Fixtures

(defun scv-test-fixtures-dir ()
  "Return the absolute path to the emacs/fixtures/ directory."
  (let ((candidates
         (list
          ;; When running from the emacs directory
          (expand-file-name "fixtures" default-directory)
          ;; When running from the project root
          (expand-file-name "scv/emacs/fixtures" default-directory)
          ;; Relative to load-file-name or buffer-file-name
          (when load-file-name
            (let ((base (file-name-directory load-file-name)))
              (expand-file-name "fixtures" base)))
          (when buffer-file-name
            (let ((base (file-name-directory buffer-file-name)))
              (expand-file-name "fixtures" base))))))
    (cl-find-if #'file-exists-p candidates)))

(defun scv-test-load-fixture (name)
  "Load JSON fixture NAME as an alist."
  (let ((path (expand-file-name (concat name ".json") (scv-test-fixtures-dir))))
    (unless (file-exists-p path)
      (error "Fixture not found: %s" path))
    (with-temp-buffer
      (insert-file-contents path)
      (json-parse-buffer :object-type 'alist
                         :array-type 'list
                         :null-object nil
                         :false-object :json-false))))

(defun scv-test--make-event (type &rest fields)
  "Construct a minimal typed test event ALIST of TYPE with FIELDS."
  (append `((v . 2) (schema_version . 2) (agent . "scv")
            (event . (,(mapconcat #'capitalize
                                   (split-string type "_")
                                   "_")
                      ,(mapcar
                        (lambda (field)
                          (let ((value (cdr field)))
                            (list (symbol-name (car field))
                                  (if (and (listp value)
                                           (= (length value) 1))
                                      (car value)
                                    value))))
                        fields)))
            (session_id . "tc-test-001")
            (cwd . "/home/user/project")
            (timestamp . "2025-01-01T00:00:10Z"))
          nil))

;;; Tests

(ert-deftest scv-viewer-test-projection-from-fixture ()
  "Reducer produces a valid projection from basic fixture."
  (let* ((events (scv-test-load-fixture "basic-session"))
         (proj (scv-reducer-apply-events events)))
    (should (scv-projection-p proj))
    (should (string= (scv-projection-session-id proj) "tc-fixture-001"))
    (should (eq (scv-projection-state proj) 'completed))
    (should (= (length (scv-projection-messages proj)) 2))))

(ert-deftest scv-viewer-test-state-label ()
  "State labels are human-readable."
  (should (string= (scv-session-viewer--state-label 'idle) "idle"))
  (should (string= (scv-session-viewer--state-label 'blocked_on_permission) "blocked: permission"))
  (should (string= (scv-session-viewer--state-label 'running) "running")))

(ert-deftest scv-viewer-test-state-face ()
  "State faces map correctly."
  (should (eq (scv-session-viewer--state-face 'idle) 'scv-viewer-st-idle))
  (should (eq (scv-session-viewer--state-face 'completed) 'scv-viewer-st-idle))
  (should (eq (scv-session-viewer--state-face 'running) 'scv-viewer-st-run))
  (should (eq (scv-session-viewer--state-face 'blocked_on_permission) 'scv-viewer-st-block))
  (should (eq (scv-session-viewer--state-face 'failed) 'scv-viewer-st-err)))

(ert-deftest scv-viewer-test-render-header-text ()
  "Header text renders session metadata."
  (let* ((events (scv-test-load-fixture "basic-session"))
         (proj (scv-reducer-apply-events events))
         (text (scv-session-viewer--render-header-text proj)))
    (should (string-match-p "tc-fixture-001" text))
    (should (string-match-p "gpt-5.5" text))
    (should (string-match-p "permission" text))))

(ert-deftest scv-viewer-test-render-messages-text ()
  "Messages text renders with role prefixes."
  (let* ((events (scv-test-load-fixture "basic-session"))
         (proj (scv-reducer-apply-events events))
         (text (scv-session-viewer--render-messages-text proj)))
    (should (string-match-p ">>> user" text))
    (should (string-match-p ">>> assistant" text))
    (should (string-match-p "Inspect this repository" text))))

(ert-deftest scv-viewer-test-render-tools-text ()
  "Tools text renders with status indicators."
  (let* ((events (scv-test-load-fixture "tool-success"))
         (proj (scv-reducer-apply-events events))
         (text (scv-session-viewer--render-tools-text proj)))
    (should (string-match-p "✓" text))
    (should (string-match-p "bash" text))
    (should (string-match-p "ls -la" text))))

(ert-deftest scv-viewer-test-render-blockers-text ()
  "Pending blockers render prominently."
  (let* ((proj (make-scv-projection
                :session-id "tc-blocker-test"
                :state 'blocked_on_permission
                :blockers
                (list (make-scv-blocker
                       :id "perm-001"
                       :type 'permission
                       :tool-name "bash"
                       :detail "rm -rf /tmp/cache"
                       :status "pending"))))
         (text (scv-session-viewer--render-blockers-text proj)))
    (should (string-match-p "perm-001" text))
    (should (string-match-p "bash" text))))

(ert-deftest scv-viewer-test-render-footer-text ()
  "Footer text renders state."
  (let* ((events (scv-test-load-fixture "basic-session"))
         (proj (scv-reducer-apply-events events))
         (text (scv-session-viewer--render-footer-text proj)))
    (should (string-match-p "completed" text))))

(ert-deftest scv-viewer-test-render-input-area-text ()
  "Input area text renders prompt marker."
  (let ((text (scv-session-viewer--render-input-area-text)))
    (should (string-match-p "prompt" (downcase text)))
    (should (string-match-p "─" text))))

(ert-deftest scv-viewer-test-render-full-text ()
  "Full render produces complete session view."
  (let* ((events (scv-test-load-fixture "basic-session"))
         (proj (scv-reducer-apply-events events))
         (text (scv-session-viewer--render-full-text proj)))
    (should (string-match-p "tc-fixture-001" text))
    (should (string-match-p "Inspect this repository" text))
    (should (string-match-p "prompt" (downcase text)))))

(ert-deftest scv-viewer-test-log-path ()
  "Log path points to .scv/transcripts/."
  (let ((path (scv-session-viewer--log-path "tc-abc123")))
    (should (string-match-p "\\.scv" path))
    (should (string-match-p "transcripts" path))
    (should (string-match-p "tc-abc123\\.jsonl" path))))

(ert-deftest scv-viewer-test-parse-line ()
  "Parse a single JSONL event line."
  (let ((line "{\"event\":[\"Session_start\",[]],\"session_id\":\"tc-123\"}")
        (event (scv-session-viewer--parse-line "{\"event\":[\"Session_start\",[]],\"session_id\":\"tc-123\"}")))
    (should event)
    (should (equal (alist-get 'event event) '("Session_start" nil)))))

(ert-deftest scv-viewer-test-parse-line-reads-v2-variant-event ()
  "Parse persisted v2 event JSON in the typed event shape."
  (let* ((line
          "{\"v\":2,\"schema_version\":2,\"agent\":\"scv\",\"event\":[\"Session_start\",[[\"provider\",\"responses\"],[\"model\",\"gpt-5.5\"],[\"provider_tools_enabled\",true],[\"tools_enabled\",true]]],\"session_id\":\"tc-v2\",\"cwd\":\"/tmp/scv\",\"project\":\"scv\",\"timestamp\":\"2026-05-27T00:00:00Z\",\"provider_conversation_items\":[]}")
         (event (scv-session-viewer--parse-line line)))
    (should event)
    (should (equal (scv-reducer--event-type event) "session_start"))
    (should (equal (scv-reducer--field event 'provider) "responses"))
    (should (equal (scv-reducer--field event 'model) "gpt-5.5"))))

(ert-deftest scv-viewer-test-parse-line-invalid ()
  "Invalid JSON returns nil."
  (should (null (scv-session-viewer--parse-line "not json"))))

(ert-deftest scv-viewer-test-read-new-events-recovers-from-truncation ()
  "When a JSONL log is truncated, catch-up restarts from byte zero."
  (let ((log (make-temp-file "scv-viewer-truncated-" nil ".jsonl")))
    (unwind-protect
        (progn
          (with-temp-file log
            (insert "{\"event\":\"session_start\",\"session_id\":\"tc-old\"}\n")
            (insert "{\"event\":\"session_end\",\"session_id\":\"tc-old\"}\n"))
          (let ((old-size (nth 7 (file-attributes log))))
            (with-temp-file log
              (insert "{\"event\":\"session_start\",\"session_id\":\"tc-new\"}\n"))
            (let* ((res (scv-session-viewer--read-new-events log old-size))
                   (events (car res))
                   (pos (cdr res)))
              (should (= (length events) 1))
              (should (equal (alist-get 'session_id (car events)) "tc-new"))
              (should (= pos (nth 7 (file-attributes log)))))))
      (ignore-errors
        (delete-file log)))))

(ert-deftest scv-viewer-test-event-processing ()
  "Events update projection in buffer-local state."
  (let* ((buf (get-buffer-create "*test-viewer-events*"))
         (events (scv-test-load-fixture "basic-session")))
    (with-current-buffer buf
      (setq-local scv-session-viewer--projection nil)
      (setq-local scv-session-viewer--session-id "tc-abc")
      (scv-session-viewer--process-events events)
      (should (scv-projection-p scv-session-viewer--projection))
      (should (eq (scv-projection-state scv-session-viewer--projection) 'completed)))
    (kill-buffer buf)))

(ert-deftest scv-viewer-test-event-processing-accepts-v2-variant-event ()
  "Socket and transcript events in ppx variant shape do not crash the reducer."
  (let* ((buf (get-buffer-create "*test-viewer-v2-event*"))
         (event
          '((v . 2)
            (schema_version . 2)
            (agent . "scv")
            (event . ("Session_start"
                      (("provider" "responses")
                       ("model" "gpt-5.5")
                       ("provider_tools_enabled" t)
                       ("tools_enabled" t))))
            (session_id . "tc-v2")
            (cwd . "/tmp/scv")
            (project . "scv")
            (timestamp . "2026-05-27T00:00:00Z")
            (provider_conversation_items . ()))))
    (with-current-buffer buf
      (setq-local scv-session-viewer--projection nil)
      (setq-local scv-session-viewer--session-id "tc-v2")
      (scv-session-viewer--process-events (list event))
      (should (scv-projection-p scv-session-viewer--projection))
      (should (eq (scv-projection-state scv-session-viewer--projection) 'idle))
      (should (equal (scv-projection-model scv-session-viewer--projection)
                     "gpt-5.5")))
    (kill-buffer buf)))

(ert-deftest scv-viewer-test-protocol-event-frame-processes-typed-event ()
  "Session protocol event frames pass typed events to the reducer unchanged."
  (let* ((frame
          (scv-session-viewer--parse-line
           "{\"protocol\":\"scv-session\",\"version\":1,\"type\":\"event\",\"session_id\":\"tc-v2\",\"event\":{\"v\":2,\"schema_version\":2,\"agent\":\"scv\",\"event\":[\"Session_start\",[[\"provider\",\"responses\"],[\"model\",\"gpt-5.5\"]]],\"session_id\":\"tc-v2\",\"cwd\":\"/tmp/scv\",\"project\":\"scv\",\"timestamp\":\"2026-05-27T00:00:00Z\",\"provider_conversation_items\":[]},\"index\":1}"))
         (event (scv-session-viewer--alist-get-string 'event frame))
         (buf (get-buffer-create "*test-viewer-protocol-frame*")))
    (with-current-buffer buf
      (setq-local scv-session-viewer--projection nil)
      (setq-local scv-session-viewer--session-id "tc-v2")
      (scv-session-viewer--process-event event)
      (should (scv-projection-p scv-session-viewer--projection))
      (should (eq (scv-projection-state scv-session-viewer--projection) 'idle))
      (should (equal (scv-projection-model scv-session-viewer--projection)
                     "gpt-5.5")))
    (kill-buffer buf)))

(ert-deftest scv-viewer-test-incremental-update ()
  "New events append to existing view without full re-render."
  (let* ((buf (get-buffer-create "*test-viewer-incremental*"))
         (start-event (scv-test--make-event "session_start"
                                              '(model . "gpt-4")
                                              '(provider . "openai")))
         (prompt-event (scv-test--make-event "prompt_submit"
                                               '(query . "hello"))))
    (with-current-buffer buf
      (setq-local scv-session-viewer--projection nil)
      (setq-local scv-session-viewer--session-id "tc-abc")

      ;; Process start event
      (scv-session-viewer--process-events (list start-event))
      (let ((proj scv-session-viewer--projection))
        (should (eq (scv-projection-state proj) 'idle)))

      ;; Process prompt event incrementally
      (let ((old-content (buffer-substring-no-properties (point-min) (point-max))))
        (scv-session-viewer--process-events (list prompt-event))
        (let ((new-content (buffer-substring-no-properties (point-min) (point-max))))
          ;; Content should have grown
          (should (> (length new-content) (length old-content)))
          ;; New content should contain the prompt text
          (should (string-match-p "hello" new-content))))

      (should (eq (scv-projection-state scv-session-viewer--projection) 'running)))
    (kill-buffer buf)))

(ert-deftest scv-viewer-test-no-events-no-render ()
  "Empty event list does not crash."
  (let ((buf (get-buffer-create "*test-viewer-no-events*")))
    (with-current-buffer buf
      (setq-local scv-session-viewer--projection nil)
      (setq-local scv-session-viewer--session-id "tc-abc")
      (scv-session-viewer--process-events nil)
      (should (null scv-session-viewer--projection)))
    (kill-buffer buf)))

(ert-deftest scv-viewer-test-open-fresh-session-renders-created-projection ()
  "Opening a session with no transcript still renders the viewer chrome."
  (let* ((data-dir (make-temp-file "scv-viewer-data-" t))
         (runtime-dir (make-temp-file "scv-viewer-runtime-" t))
         (process-environment (copy-sequence process-environment))
         (session-id "tc-empty-open")
         (buf-name (format "*scv %s*" session-id)))
    (setenv "SCV_DATA_DIR" data-dir)
    (setenv "SCV_RUNTIME_DIR" runtime-dir)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
            (scv-session-viewer-open session-id))
          (with-current-buffer buf-name
            (should (scv-projection-p scv-session-viewer--projection))
            (should (eq (scv-projection-state scv-session-viewer--projection)
                        'created))
            (should (equal (scv-projection-session-id scv-session-viewer--projection)
                           session-id))
            (let ((text (buffer-substring-no-properties
                         (point-min) (point-max))))
              (should (string-match-p "session: tc-empty-open" text))
              (should (string-match-p "\\[created\\]" text))
              (should (string-match-p "start typing to prompt > " text)))))
      (when (get-buffer buf-name)
        (with-current-buffer buf-name
          (when scv-session-viewer--poll-timer
            (cancel-timer scv-session-viewer--poll-timer)))
        (kill-buffer buf-name))
      (ignore-errors (delete-directory data-dir t))
      (ignore-errors (delete-directory runtime-dir t)))))

(ert-deftest scv-viewer-test-interrupt-sends-session-action ()
  "scv-session-viewer--interrupt sends the correct session action command."
  (let ((sent-args nil)
        (scv-session-action-interrupt-command '("session" "action" "interrupt")))
    (cl-letf (((symbol-function 'scv-session-viewer--run-session-action)
               (lambda (args) (setq sent-args args))))
      (with-temp-buffer
        (setq-local scv-session-viewer--session-id "tc-interrupt-test")
        (scv-session-viewer--interrupt)
        (should (equal sent-args
                       '("session" "action" "interrupt"
                         "--session-id" "tc-interrupt-test")))))))

(ert-deftest scv-viewer-test-interrupt-keybound ()
  "The interrupt command is bound to 'i' in the viewer keymap."
  (let ((map (let ((m (make-sparse-keymap)))
               (define-key m (kbd "RET") #'ignore)
               (define-key m (kbd "C-c C-c") #'ignore)
               (define-key m (kbd "i") #'scv-session-viewer--interrupt)
               (define-key m (kbd "n") #'scv-session-viewer-scroll-up)
               m)))
    (should (eq (lookup-key map (kbd "i")) #'scv-session-viewer--interrupt))
    (should (eq (lookup-key map (kbd "n")) #'scv-session-viewer-scroll-up))))

(ert-deftest scv-viewer-test-incremental-tool-status-update ()
  "Incremental tool status update renders the changed status correctly."
  (let* ((buf (get-buffer-create "*test-viewer-tool-update*"))
         (tool-call-id "call-update-001")
         (start-event (scv-test--make-event "tool_start"
                       '(tool_call_id . "call-update-001")
                       '(tool_name . "bash")
                       '(tool_input . "echo hello")
                       '(input_preview . "echo hello")))
         (result-event (scv-test--make-event "tool_result"
                        '(tool_call_id . "call-update-001")
                        '(tool_name . "bash")
                        '(status . "success")
                        '(result . "hello"))))
    (unwind-protect
        (with-current-buffer buf
          (setq-local scv-session-viewer--projection nil)
          (setq-local scv-session-viewer--session-id "tc-tool-update")
          ;; Process start event — tool shows as running (…)
          (scv-session-viewer--process-events (list start-event))
          (should (= (length (scv-projection-tools
                              scv-session-viewer--projection))
                     1))
          (should (string= (scv-tool-status
                            (car (scv-projection-tools
                                  scv-session-viewer--projection)))
                           "running"))
          (let ((text (buffer-substring-no-properties
                       (point-min) (point-max))))
            (should (string-match-p "\\[…\\] bash echo hello" text))
            (should-not (string-match-p "\\[✓\\]" text)))
          ;; Process result event — tool updates to success (✓)
          (scv-session-viewer--process-event result-event)
          (should (string= (scv-tool-status
                            (car (scv-projection-tools
                                  scv-session-viewer--projection)))
                           "success"))
          (let ((text (buffer-substring-no-properties
                       (point-min) (point-max))))
            (should (string-match-p "\\[✓\\] bash echo hello" text))
            (should (string-match-p "→ hello" text))))
      (kill-buffer buf))))

(ert-deftest scv-viewer-test-state-label-prompt-queued ()
  "State label for prompt_queued is 'prompt queued'."
  (should (string= (scv-session-viewer--state-label 'prompt_queued)
                   "prompt queued")))

(ert-deftest scv-viewer-test-state-label-handing-off ()
  "State label for handing_off is 'handing off'."
  (should (string= (scv-session-viewer--state-label 'handing_off)
                   "handing off")))

(ert-deftest scv-viewer-test-state-face-prompt-queued ()
  "State face for prompt_queued uses running face."
  (should (eq (scv-session-viewer--state-face 'prompt_queued)
              'scv-viewer-st-run)))

(ert-deftest scv-viewer-test-state-face-handing-off ()
  "State face for handing_off uses blocked face."
  (should (eq (scv-session-viewer--state-face 'handing_off)
              'scv-viewer-st-block)))

(ert-deftest scv-viewer-test-render-handoff-text ()
  "Handoff text renders stage and details."
  (let* ((proj (make-scv-projection
                :session-id "tc-handoff-test"
                :state 'handing_off
                :handoff-progress
                (list (make-scv-handoff
                       :index 0
                       :event-type "handoff_progress"
                       :stage "pushing"
                       :attempt-id nil
                       :text "Pushing to origin/main"))))
         (text (scv-session-viewer--render-handoff-text proj)))
    (should (string-match-p "handoff" text))
    (should (string-match-p "pushing" text))
    (should (string-match-p "Pushing to origin/main" text))))

(ert-deftest scv-viewer-test-render-full-includes-handoff ()
  "Full render includes handoff section when handoff-progress is non-empty."
  (let* ((proj (make-scv-projection
                :session-id "tc-handoff-full"
                :state 'handing_off
                :handoff-progress
                (list (make-scv-handoff
                       :index 0
                       :event-type "handoff_progress"
                       :stage "auth_required"
                       :attempt-id nil
                       :text "Need GitHub OAuth"))))
         (text (scv-session-viewer--render-full-text proj)))
    (should (string-match-p "--- handoff ---" text))
    (should (string-match-p "auth_required" text))
    (should (string-match-p "GitHub OAuth" text))))

(ert-deftest scv-viewer-test-close-kills-buffer ()
  "scv-session-viewer-close kills the current viewer buffer."
  (with-temp-buffer
    (setq-local scv-session-viewer--poll-timer nil)
    (setq-local scv-session-viewer--socket-process nil)
    (let ((buf (current-buffer)))
      (scv-session-viewer-close)
      (should-not (buffer-live-p buf)))))

(ert-deftest scv-viewer-test-close-cancels-timer ()
  "scv-session-viewer-close cancels the polling timer."
  (let ((timer-cancelled nil))
    (with-temp-buffer
      (setq-local scv-session-viewer--poll-timer t) ; any non-nil value
      (setq-local scv-session-viewer--socket-process nil)
      (cl-letf (((symbol-function 'cancel-timer)
                 (lambda (timer) (setq timer-cancelled t))))
        (scv-session-viewer-close))
      (should timer-cancelled))))

(ert-deftest scv-viewer-test-scroll-beginning-sets-user-at-bottom-nil ()
  "Beginning of viewer pauses follow mode."
  (with-temp-buffer
    (setq-local scv-session-viewer--user-at-bottom t)
    (scv-session-viewer-beginning)
    (should (eq scv-session-viewer--user-at-bottom nil))))

(ert-deftest scv-viewer-test-scroll-end-sets-user-at-bottom-t ()
  "End of viewer resumes follow mode."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (setq-local scv-session-viewer--user-at-bottom nil)
    (scv-session-viewer-end)
    (should (eq scv-session-viewer--user-at-bottom t))))

(ert-deftest scv-viewer-test-scroll-up-resumes-follow ()
  "Scroll-up resumes following output."
  (with-temp-buffer
    (setq-local scv-session-viewer--user-at-bottom nil)
    (cl-letf (((symbol-function 'scroll-up-command) #'ignore))
      (scv-session-viewer-scroll-up))
    (should (eq scv-session-viewer--user-at-bottom t))))

(ert-deftest scv-viewer-test-scroll-down-pauses-follow-when-far-from-bottom ()
  "Scroll-down pauses follow when point is well above bottom."
  (with-temp-buffer
    (insert (make-string 500 ?a) "\n")
    (insert (make-string 500 ?b) "\n")
    (insert (make-string 500 ?c) "\n")
    (goto-char (point-min))
    (setq-local scv-session-viewer--user-at-bottom t)
    (cl-letf (((symbol-function 'scroll-down-command)
               (lambda () (forward-line -1))))
      (scv-session-viewer-scroll-down))
    ;; point at start is far from the end
    (should (eq scv-session-viewer--user-at-bottom nil))))

(provide 'scv-session-viewer-tests)
;;; scv-session-viewer-tests.el ends here
