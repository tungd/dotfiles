;;; vterm-agent-tests.el --- Tests for vterm-agent -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'vterm-agent)

(ert-deftest vterm-agent-parse-osc-9-notification ()
  (let ((event (vterm-agent--parse-osc-payload
                "9;Task complete"
                "\e]9;Task complete\a"
                "local:codex")))
    (should (equal (plist-get event :session-id) "local:codex"))
    (should (eq (plist-get event :kind) 'complete))
    (should (null (plist-get event :title)))
    (should (equal (plist-get event :body) "Task complete"))))

(ert-deftest vterm-agent-parse-osc-777-notify ()
  (let ((event (vterm-agent--parse-osc-payload
                "777;notify;Claude Code;Needs approval"
                "\e]777;notify;Claude Code;Needs approval\e\\"
                "ssh:dev:claude")))
    (should (equal (plist-get event :session-id) "ssh:dev:claude"))
    (should (eq (plist-get event :kind) 'request))
    (should (equal (plist-get event :title) "Claude Code"))
    (should (equal (plist-get event :body) "Needs approval"))))

(ert-deftest vterm-agent-ignores-osc-9-progress ()
  (should-not
   (vterm-agent--parse-osc-payload
    "9;4;1;50"
    "\e]9;4;1;50\a"
    "local:codex")))

(ert-deftest vterm-agent-scans-split-osc-sequence ()
  (let ((events nil)
        (vterm-agent-notification-function nil))
    (with-temp-buffer
      (setq-local vterm-agent-session-id "local:codex")
      (add-hook 'vterm-agent-event-hook
                (lambda (event) (push event events))
                nil t)
      (vterm-agent--scan-output "\e]777;notify;Codex;Task")
      (vterm-agent--scan-output " finished\a")
      (should (= (length events) 1))
      (should (eq (plist-get (car events) :kind) 'complete))
      (should (equal (plist-get (car events) :body) "Task finished")))))

(ert-deftest vterm-agent-send-to-session-uses-bracketed-paste ()
  (let ((calls nil)
        (vterm-agent-submit-keys '("Enter")))
    (cl-letf (((symbol-function 'vterm-agent--tmux)
               (lambda (host args &optional input)
                 (push (list host args input) calls)
                 "")))
      (vterm-agent--send-to-session
       (make-vterm-agent--session :host nil :name "codex")
       "line one\nline two")
      (setq calls (nreverse calls))
      (should (equal calls
                     '((nil
                        ("load-buffer" "-b" "vterm-agent" "-")
                        "line one\nline two")
                       (nil
                        ("paste-buffer" "-p" "-r" "-b" "vterm-agent"
                         "-t" "codex")
                        nil)
                       (nil
                        ("send-keys" "-t" "codex" "Enter")
                        nil)))))))

(ert-deftest vterm-agent-kill-session-kills-tmux-and-forgets-session ()
  (let ((calls nil)
        (vterm-agent--sessions (make-hash-table :test 'equal)))
    (let ((session (vterm-agent--remember-session nil "codex" "detached")))
      (cl-letf (((symbol-function 'vterm-agent--tmux)
                 (lambda (host args &optional input)
                   (push (list host args input) calls)
                   "")))
        (should (equal (vterm-agent--kill-session session)
                       "local:codex"))
        (should-not (gethash "local:codex" vterm-agent--sessions))
        (should (equal (nreverse calls)
                       '((nil
                          ("kill-session" "-t" "codex")
                          nil))))))))

(ert-deftest vterm-agent-capture-session-output-normalizes-pane ()
  (let ((calls nil)
        (vterm-agent-capture-max-lines 3))
    (cl-letf (((symbol-function 'vterm-agent--tmux)
               (lambda (host args &optional input)
                 (push (list host args input) calls)
                 "one  \r\ntwo\e[31m red\e[0m  \n\n")))
      (should (equal
               (vterm-agent--capture-session-output
                (make-vterm-agent--session :host nil :name "codex"))
               "one\n\ntwo red"))
      (should (equal (nreverse calls)
                     '((nil
                        ("capture-pane" "-p" "-J" "-S" "-3" "-t" "codex")
                        nil)))))))

(ert-deftest vterm-agent-render-run-includes-captured-output ()
  (with-temp-buffer
    (vterm-agent-cells-mode)
    (setq-local vterm-agent-session-id "local:codex")
    (insert "# %%\nhello")
    (let* ((run-id "1")
           (run (list :id run-id :session-id "local:codex"
                      :status "running"
                      :message "Prompt sent; capturing tmux output"
                      :output "assistant reply\nnext line"
                      :result-marker (copy-marker (point-max) t))))
      (puthash run-id run vterm-agent--runs)
      (vterm-agent--render-run run-id)
      (should (string-match-p
               "# => vterm-agent run 1 \\[running\\]"
               (buffer-string)))
      (should (string-match-p "# --- output ---" (buffer-string)))
      (should (string-match-p "#    assistant reply" (buffer-string)))
      (should (string-match-p "#    next line" (buffer-string))))))

(ert-deftest vterm-agent-send-and-new-cell-keeps-result-before-next-cell ()
  (with-temp-buffer
    (vterm-agent-cells-mode)
    (setq-local vterm-agent-session-id "local:codex")
    (vterm-agent--remember-session nil "codex")
    (insert "# %%\nHello")
    (goto-char (point-max))
    (cl-letf (((symbol-function 'vterm-agent--send-to-session)
               (lambda (_session _text) nil))
              ((symbol-function 'vterm-agent--schedule-output-capture)
               (lambda (_run-id) nil)))
      (vterm-agent-send-cell-and-new-cell)
      (let* ((text (buffer-string))
             (result-pos (string-match "# => vterm-agent run 1 \\[running\\]" text))
             (next-cell-pos (string-match "\n# %%\n\\'" text)))
        (should result-pos)
        (should next-cell-pos)
        (should (< result-pos next-cell-pos)))
      (let ((run (gethash "1" vterm-agent--runs)))
        (setq run (plist-put run :output "assistant reply"))
        (puthash "1" run vterm-agent--runs))
      (vterm-agent--render-run "1")
      (let* ((text (buffer-string))
             (output-pos (string-match "#    assistant reply" text))
             (next-cell-pos (string-match "\n# %%\n\\'" text)))
        (should output-pos)
        (should next-cell-pos)
        (should (< output-pos next-cell-pos))))))

(ert-deftest vterm-agent-posframe-format-includes-status-and-session ()
  (let ((vterm-agent-posframe-width 32)
        (vterm-agent-posframe-max-body-lines 2))
    (let ((text (substring-no-properties
                 (vterm-agent--format-posframe-notification
                  '(:session-id "local:codex-dotfiles"
                    :kind complete
                    :title "Codex"
                    :body "Task completed after a fairly long assistant response.")))))
      (should (string-match-p "✓ Codex" text))
      (should (string-match-p "local:codex-dotfiles" text))
      (should (string-match-p "Task completed" text))
      (should-not (string-match-p "\n\n" text)))))

(provide 'vterm-agent-tests)

;;; vterm-agent-tests.el ends here
