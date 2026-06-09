;;; scv-reducer-tests.el --- Unit tests for scv-reducer -*- lexical-binding: t; -*-

(require 'ert)
(require 'scv-reducer)

;;; Helpers

(defun scv-test--fixtures-dir ()
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

(defun scv-test-load-events (fixture-name)
  "Load decoded events from a fixture file in emacs/fixtures/."
  (let ((dir (scv-test--fixtures-dir)))
    (unless dir
      (error "Fixtures directory not found"))
    (let ((path (expand-file-name fixture-name dir)))
      (unless (file-exists-p path)
        (error "Fixture not found: %s" path))
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (let ((parsed (json-parse-buffer :array-type 'list
                                         :object-type 'alist
                                         :null-object nil
                                         :false-object :json-false)))
          (unless (listp parsed)
            (error "Fixture %s did not parse as a JSON list (got %s)" fixture-name (type-of parsed)))
          parsed)))))

(defun scv-reducer-test--event (event-type &rest fields)
  "Construct a minimal typed test event ALIST of EVENT-TYPE with FIELDS."
  (append `((v . 2) (schema_version . 2) (agent . "scv")
            (event . (,(mapconcat #'capitalize
                                   (split-string event-type "_")
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
            (session_id . "tc-fixture-001")
            (cwd . "/home/user/project")
            (timestamp . "2025-01-01T00:00:10Z"))
          nil))

;;; Tests

(ert-deftest scv-reducer-test-empty-session ()
  "Empty event list returns a created-state projection."
  (let ((proj (scv-reducer-apply-events nil "tc-empty")))
    (should (eq (scv-projection-state proj) 'created))
    (should (string= (scv-projection-session-id proj) "tc-empty"))
    (should (null (scv-projection-messages proj)))
    (should (null (scv-projection-tools proj)))
    (should (null (scv-projection-blockers proj)))))

(ert-deftest scv-reducer-test-basic-session ()
  "Basic session: start → prompt → response."
  (let* ((events (scv-test-load-events "basic-session.json"))
         (proj (scv-reducer-apply-events events)))
    (should (eq (scv-projection-state proj) 'completed))
    (should (string= (scv-projection-session-id proj) "tc-fixture-001"))
    (should (string= (scv-projection-model proj) "gpt-5.5"))
    (should (string= (scv-projection-provider proj) "openai"))
    (should (string= (scv-projection-cwd proj) "/home/user/project"))
    (should (= (length (scv-projection-messages proj)) 2))
    (should (null (scv-projection-tools proj)))
    (should (null (scv-projection-blockers proj)))
    ;; First message: user
    (let ((msg (nth 0 (scv-projection-messages proj))))
      (should (string= (scv-message-role msg) "user"))
      (should (string= (scv-message-content msg) "Inspect this repository")))
    ;; Second message: assistant
    (let ((msg (nth 1 (scv-projection-messages proj))))
      (should (string= (scv-message-role msg) "assistant"))
      (should (string= (scv-message-content msg) "I'll inspect the repository structure for you.")))))

(ert-deftest scv-reducer-test-tool-success ()
  "Tool call that succeeds."
  (let* ((events (scv-test-load-events "tool-success.json"))
         (proj (scv-reducer-apply-events events)))
    (should (eq (scv-projection-state proj) 'completed))
    (should (= (length (scv-projection-tools proj)) 1))
    (let ((tool (nth 0 (scv-projection-tools proj))))
      (should (string= (scv-tool-name tool) "bash"))
      (should (string= (scv-tool-input-preview tool) "ls -la"))
      (should (string= (scv-tool-status tool) "success"))
      (should (string= (scv-tool-index tool) "call-abc123"))
      (should (string-match "total 48" (scv-tool-result tool))))))

(ert-deftest scv-reducer-test-tool-failure ()
  "Tool call that fails with an error."
  (let* ((events (scv-test-load-events "tool-failure.json"))
         (proj (scv-reducer-apply-events events)))
    (should (= (length (scv-projection-tools proj)) 1))
    (let ((tool (nth 0 (scv-projection-tools proj))))
      (should (string= (scv-tool-status tool) "failure"))
      (should (string= (scv-tool-error tool) "Build failed: module not found"))
      (should (string-match "stderr:" (scv-tool-result tool))))))

(ert-deftest scv-reducer-test-permission-flow ()
  "Permission requested, allowed, then tool executes."
  (let* ((events (scv-test-load-events "permission-flow.json"))
         (proj (scv-reducer-apply-events events)))
    (should (= (length (scv-projection-blockers proj)) 1))
    (let ((blocker (nth 0 (scv-projection-blockers proj))))
      (should (string= (scv-blocker-id blocker) "perm-001"))
      (should (eq (scv-blocker-type blocker) 'permission))
      (should (string= (scv-blocker-tool-name blocker) "bash"))
      (should (string= (scv-blocker-status blocker) "allowed"))
      (should (string= (scv-blocker-decision blocker) "allow_once")))
    (should (= (length (scv-projection-tools proj)) 1))
    (let ((tool (nth 0 (scv-projection-tools proj))))
      (should (string= (scv-tool-status tool) "success")))))

(ert-deftest scv-reducer-test-interrupted ()
  "Session transitions to interrupted state."
  (let* ((events (scv-test-load-events "reasoning-turn.json"))
         (proj (scv-reducer-apply-events events)))
    (should (eq (scv-projection-state proj) 'interrupted))
    (should (= (length (scv-projection-messages proj)) 1))
    (let ((msg (nth 0 (scv-projection-messages proj))))
      (should (string= (scv-message-role msg) "user")))))

(ert-deftest scv-reducer-test-state-transitions ()
  "Verify state transitions through a session lifecycle."
  (let* ((events (scv-test-load-events "basic-session.json"))
         (proj (scv-reducer--init-state)))
    ;; After session_start
    (setq proj (scv-reducer--apply-one-event proj (nth 0 events)))
    (should (eq (scv-projection-state proj) 'idle))

    ;; After prompt_submit
    (setq proj (scv-reducer--apply-one-event proj (nth 1 events)))
    (should (eq (scv-projection-state proj) 'running))

    ;; After provider_response with completed status
    (setq proj (scv-reducer--apply-one-event proj (nth 2 events)))
    (should (eq (scv-projection-state proj) 'completed))))

(ert-deftest scv-reducer-test-set-fields ()
  "set_model, set_permission_mode, set_skills update projection fields."
  (let* ((events (scv-test-load-events "basic-session.json"))
         (extra
          (list (scv-reducer-test--event "set_model" '(model . "claude-3-opus"))
                (scv-reducer-test--event "set_permission_mode" '(mode . "auto"))
                (scv-reducer-test--event "set_skills" '(skills "debug" "test"))))
         (proj (scv-reducer-apply-events (append events extra))))
    (should (string= (scv-projection-model proj) "claude-3-opus"))
    (should (eq (scv-projection-permission-mode proj) 'auto))
    (should (equal (scv-projection-active-skills proj) '("debug" "test")))))

(ert-deftest scv-reducer-test-tool-result-event ()
  "tool_result event updates the matching tool with result and status."
  (let* ((events (list
                  (scv-reducer-test--event "tool_start"
                    '(tool_call_id . "call-abc")
                    '(tool_name . "bash")
                    '(tool_input . "ls"))
                  (scv-reducer-test--event "tool_result"
                    '(tool_call_id . "call-abc")
                    '(tool_name . "bash")
                    '(status . "success")
                    '(result . "total 12")
                    '(duration_ms . 150))))
         (proj (scv-reducer-apply-events events)))
    (should (= (length (scv-projection-tools proj)) 1))
    (let ((tool (nth 0 (scv-projection-tools proj))))
      (should (string= (scv-tool-name tool) "bash"))
      (should (string= (scv-tool-status tool) "success"))
      (should (string= (scv-tool-result tool) "total 12"))
      (should (string= (scv-tool-error tool) "")))))

(ert-deftest scv-reducer-test-tool-result-fallback-by-name ()
  "tool_result without tool_call_id falls back to matching by tool name."
  (let* ((events (list
                  (scv-reducer-test--event "tool_start"
                    '(tool_name . "file_read")
                    '(tool_input . "readme.md"))
                  (scv-reducer-test--event "tool_result"
                    '(tool_name . "file_read")
                    '(status . "success")
                    '(result . "# readme"))))
         (proj (scv-reducer-apply-events events)))
    (let ((tool (nth 0 (scv-projection-tools proj))))
      (should (string= (scv-tool-status tool) "success"))
      (should (string= (scv-tool-result tool) "# readme")))))

(ert-deftest scv-reducer-test-tool-result-failure ()
  "tool_result with failure status sets error."
  (let* ((events (list
                  (scv-reducer-test--event "tool_call"
                    '(tool_call_id . "call-fail")
                    '(tool_name . "bash")
                    '(tool_input . "false"))
                  (scv-reducer-test--event "tool_result"
                    '(tool_call_id . "call-fail")
                    '(tool_name . "bash")
                    '(status . "failure")
                    '(result . "")
                    '(error . "exit code 1"))))
         (proj (scv-reducer-apply-events events)))
    (let ((tool (nth 0 (scv-projection-tools proj))))
      (should (string= (scv-tool-status tool) "failure"))
      (should (string= (scv-tool-error tool) "exit code 1")))))

(ert-deftest scv-reducer-test-tool-result-aliases-tool-complete ()
  "tool_complete is an alias handled identically to tool_result."
  (let* ((events (list
                  (scv-reducer-test--event "tool_start"
                    '(tool_call_id . "call-x")
                    '(tool_name . "bash")
                    '(tool_input . "echo hi"))
                  (scv-reducer-test--event "tool_complete"
                    '(tool_call_id . "call-x")
                    '(tool_name . "bash")
                    '(status . "success")
                    '(result . "hi"))))
         (proj (scv-reducer-apply-events events)))
    (let ((tool (nth 0 (scv-projection-tools proj))))
      (should (string= (scv-tool-status tool) "success")))))

(ert-deftest scv-reducer-test-tool-progress-event ()
  "tool_progress event updates the matching tool with a progress message."
  (let* ((events (list
                  (scv-reducer-test--event "tool_start"
                    '(tool_call_id . "call-prog")
                    '(tool_name . "bash")
                    '(tool_input . "long-build"))
                  (scv-reducer-test--event "tool_progress"
                    '(tool_call_id . "call-prog")
                    '(tool_name . "bash")
                    '(message . "compiling module foo"))))
         (proj (scv-reducer-apply-events events)))
    (should (= (length (scv-projection-tools proj)) 1))
    (let ((tool (nth 0 (scv-projection-tools proj))))
      ;; Tool should still be running
      (should (string= (scv-tool-status tool) "running"))
      ;; Progress message stored in result field as preview
      (should (string= (scv-tool-result tool) "compiling module foo")))))

(ert-deftest scv-reducer-test-tool-progress-by-name ()
  "tool_progress without tool_call_id falls back to matching by tool name."
  (let* ((events (list
                  (scv-reducer-test--event "tool_call"
                    '(tool_name . "bash")
                    '(tool_input . "sleep 10"))
                  (scv-reducer-test--event "tool_progress"
                    '(tool_name . "bash")
                    '(message . "waiting..."))))
         (proj (scv-reducer-apply-events events)))
    (let ((tool (nth 0 (scv-projection-tools proj))))
      (should (string= (scv-tool-result tool) "waiting...")))))

(ert-deftest scv-reducer-test-tool-progress-fallback-no-message ()
  "tool_progress with empty message does not overwrite existing result."
  (let* ((events (list
                  (scv-reducer-test--event "tool_start"
                    '(tool_call_id . "call-p2")
                    '(tool_name . "bash")
                    '(tool_input . "build"))
                  (scv-reducer-test--event "tool_progress"
                    '(tool_call_id . "call-p2")
                    '(tool_name . "bash")
                    '(message . "step 1"))
                  (scv-reducer-test--event "tool_progress"
                    '(tool_call_id . "call-p2")
                    '(tool_name . "bash")
                    '(message . ""))))
         (proj (scv-reducer-apply-events events)))
    (let ((tool (nth 0 (scv-projection-tools proj))))
      ;; Second empty progress should not overwrite
      (should (string= (scv-tool-result tool) "step 1")))))

(ert-deftest scv-reducer-test-idle-prompt ()
  "idle_prompt sets state to idle."
  (let* ((events (list (scv-reducer-test--event "idle_prompt")))
         (proj (scv-reducer-apply-events events)))
    (should (eq (scv-projection-state proj) 'idle))))

(ert-deftest scv-reducer-test-backend-disconnected ()
  "backend_disconnected sets state to failed."
  (let* ((events (list (scv-reducer-test--event "backend_disconnected")))
         (proj (scv-reducer-apply-events events)))
    (should (eq (scv-projection-state proj) 'failed))))

(ert-deftest scv-reducer-test-waiting-for-input ()
  "waiting_for_input sets state to blocked_on_question."
  (let* ((events (list (scv-reducer-test--event "waiting_for_input"
                        '(request_id . "req-001")
                        '(prompt . "is this ok?"))))
         (proj (scv-reducer-apply-events events)))
    (should (eq (scv-projection-state proj) 'blocked_on_question))))

(ert-deftest scv-reducer-test-handing-off-state ()
  "handoff_progress sets state to handing_off."
  (let* ((events (list (scv-reducer-test--event "handoff_progress"
                        '(stage . "pushing")
                        '(text . "Pushing to origin/main"))))
         (proj (scv-reducer-apply-events events)))
    (should (eq (scv-projection-state proj) 'handing_off))
    (should (= (length (scv-projection-handoff-progress proj)) 1))
    (let ((h (car (scv-projection-handoff-progress proj))))
      (should (string= (scv-handoff-stage h) "pushing"))
      (should (string= (scv-handoff-text h) "Pushing to origin/main")))))

(provide 'scv-reducer-tests)
;;; scv-reducer-tests.el ends here
