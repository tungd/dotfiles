;;; scv-tests.el --- Tests for scv Emacs integration -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'scv)

(defun scv-test--write-script (body)
  "Write BODY to a temporary shell script and return its path."
  (let ((path (make-temp-file "scv-test" nil ".sh")))
    (with-temp-file path
      (insert "#!/bin/sh\n" body))
    (set-file-modes path #o700)
    path))

(defun scv-test--wait-process (process)
  "Wait for PROCESS to exit and run its sentinel."
  (while (process-live-p process)
    (accept-process-output process 0.1))
  (accept-process-output process 0.1))

(ert-deftest scv-prompt-draft-buffer-is-text-mode-and-persistent ()
  (let ((scv--prompt-draft-buffers (make-hash-table :test #'equal)))
    (let ((buffer (scv--prompt-draft-buffer "session-1" "local")))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (should (eq major-mode 'text-mode))
              (should scv-prompt-draft-mode)
              (should (equal scv-session-id "session-1"))
              (should (equal scv-origin "local")))
            (should (eq buffer (scv--prompt-draft-buffer "session-1" "local"))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest scv-prompt-draft-buffer-uses-submit-keybinding ()
  (let ((scv--prompt-draft-buffers (make-hash-table :test #'equal)))
    (let ((buffer (scv--prompt-draft-buffer "session-1" "local")))
      (unwind-protect
          (with-current-buffer buffer
            (should (eq (key-binding (kbd "C-c C-c")) #'scv-prompt-submit))
            (should (eq (key-binding (kbd "C-c C-f"))
                        #'scv-prompt-insert-file))
            (should (eq (key-binding (kbd "C-c C-k")) #'scv-prompt-clear))
            (should (eq (key-binding (kbd "C-c C-v"))
                        #'scv-prompt-jump-to-viewer)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest scv-prompt-submit-command-uses-action-submit-argv ()
  (let ((scv-executable "/bin/echo")
        (scv-session-action-submit-command '("session" "action" "submit")))
    (should
     (equal (scv--prompt-submit-command "session-1" "auto")
            '("/bin/echo"
              "session" "action" "submit"
              "--session-id" "session-1"
              "--kind" "auto")))))

(ert-deftest scv-set-skills-args-uses-session-action-set-skills-argv ()
  (let ((scv-executable "/bin/echo")
        (scv-session-action-set-skills-command
         '("session" "action" "set-skills")))
    (should
     (equal (scv--set-skills-args "session-1" '("skill-a" "skill-b"))
            '("session" "action" "set-skills"
              "--session-id" "session-1"
              "--skill" "skill-a"
              "--skill" "skill-b")))))

(ert-deftest scv-session-viewer-open-link-at-point-opens-http-url ()
  (let ((opened-url nil))
    (with-temp-buffer
      (insert "open https://example.test/docs for reference")
      (goto-char (point-min))
      (search-forward "https://example.test/docs")
      (backward-char 1)
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url)
                   (setq opened-url url))))
        (scv-session-viewer-open-link-at-point)
        (should (equal opened-url "https://example.test/docs"))))))

(ert-deftest scv-session-viewer-open-link-at-point-opens-file-uri-with-line ()
  (let ((tmpfile (make-temp-file "scv-file-link")))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "line one\nline two\nline three"))
          (with-temp-buffer
            (insert (format "open file://%s#L2" tmpfile))
            (goto-char (point-min))
            (search-forward (format "file://%s#L2" tmpfile))
            (backward-char 1)
            (scv-session-viewer-open-link-at-point)
            (should (equal (buffer-file-name) (expand-file-name tmpfile)))
            (should (= (line-number-at-pos) 2))
            (kill-buffer (current-buffer))))
      (ignore-errors (delete-file tmpfile)))))

(ert-deftest scv-session-viewer-open-link-at-point-opens-relative-file-with-line ()
  (let* ((root (make-temp-file "scv-relative-link-dir" t))
         (tmpfile (expand-file-name "notes.txt" root)))
    (unwind-protect
        (progn
          (with-temp-file tmpfile
            (insert "first\nsecond\nthird"))
          (let ((default-directory root))
            (with-temp-buffer
              (setq-local default-directory root)
              (insert "./notes.txt:3")
              (goto-char (point-min))
              (search-forward "./notes.txt:3")
              (backward-char 1)
              (scv-session-viewer-open-link-at-point)
              (should (equal (buffer-file-name) (expand-file-name "notes.txt" root)))
              (should (= (line-number-at-pos) 3))
              (kill-buffer (current-buffer)))))
      (ignore-errors (delete-file tmpfile))
      (ignore-errors (delete-directory root t)))))

(ert-deftest scv-session-viewer-open-link-at-point-signals-error-when-none-found ()
  (with-temp-buffer
    (insert "plain output with no links")
    (goto-char (point-min))
    (should-error (scv-session-viewer-open-link-at-point))))

(ert-deftest scv-generate-session-id-uses-tc-prefix ()
  (let ((session-id (scv-generate-session-id)))
    (should (string-match-p "\\`tc-[0-9a-f]\\{16\\}\\'" session-id))))

(ert-deftest scv-new-session-generates-id-and-opens-viewer ()
  (let (opened)
    (cl-letf (((symbol-function 'scv-generate-session-id)
               (lambda () "tc-generated"))
              ((symbol-function 'scv-open-session-viewer)
               (lambda (session-id origin)
                 (setq opened (list session-id origin)))))
      (should (equal (scv-new-session) "tc-generated"))
      (should (equal opened '("tc-generated" "local"))))))

(ert-deftest scv-prompt-submit-kind-sends-stdin-and-clears-on-success ()
  (let* ((payload-file (make-temp-file "scv-prompt-payload"))
         (args-file (make-temp-file "scv-prompt-args"))
         (script
          (scv-test--write-script
           (format "printf '%%s\\n' \"$*\" > %s\ncat > %s\n"
                   (shell-quote-argument args-file)
                   (shell-quote-argument payload-file))))
         (scv-executable script)
         (scv--prompt-draft-buffers (make-hash-table :test #'equal))
         process
         draft-window
         draft)
    (unwind-protect
        (progn
          (delete-other-windows)
          (setq draft (scv--prompt-draft-buffer "session-1" "local"))
          (setq draft-window (display-buffer-below-selected draft nil))
          (with-current-buffer draft
            (erase-buffer)
            (insert "hello\nworld")
            (scv-prompt-submit-kind "auto")
            (setq process scv--prompt-submit-process)
            (scv-test--wait-process process)
            (should (equal (buffer-string) "")))
          (should-not (window-live-p draft-window))
          (with-temp-buffer
            (insert-file-contents payload-file)
            (should (equal (buffer-string) "hello\nworld")))
          (with-temp-buffer
            (insert-file-contents args-file)
            (should
             (string-match-p
              "session action submit --session-id session-1 --kind auto"
              (buffer-string)))))
      (when (buffer-live-p draft)
        (kill-buffer draft))
      (mapc (lambda (path) (ignore-errors (delete-file path)))
            (list payload-file args-file script)))))

(ert-deftest scv-prompt-submit-kind-keeps-draft-on-failure ()
  (let* ((script
          (scv-test--write-script
           "cat >/dev/null\nprintf 'submit failed' >&2\nexit 7\n"))
         (scv-executable script)
         (scv--prompt-draft-buffers (make-hash-table :test #'equal))
         process
         draft)
    (unwind-protect
        (progn
          (setq draft (scv--prompt-draft-buffer "session-1" "local"))
          (with-current-buffer draft
            (erase-buffer)
            (insert "keep me")
            (scv-prompt-submit-kind "auto")
            (setq process scv--prompt-submit-process)
            (scv-test--wait-process process)
            (should (equal (buffer-string) "keep me"))))
      (when (buffer-live-p draft)
        (kill-buffer draft))
      (ignore-errors (delete-file script)))))

(ert-deftest scv-open-session-viewer-delegates-to-native-viewer ()
  (let ((scv--viewer-buffers (make-hash-table :test #'equal))
        opened-session)
    (let ((original-require (symbol-function 'require)))
      (cl-letf (((symbol-function 'require)
                 (lambda (feature &optional filename noerror)
                   (if (eq feature 'scv-session-viewer)
                       t
                     (funcall original-require feature filename noerror))))
                ((symbol-function 'scv-session-viewer-open)
                 (lambda (session-id)
                   (setq opened-session session-id)
                   (puthash "local:session-1" (current-buffer)
                            scv--viewer-buffers))))
        (scv-open-session-viewer "session-1" "local")
        (should (equal opened-session "session-1"))))))

(ert-deftest scv-session-manager-entry-accepts-expected-json-fields ()
  (let* ((row '((origin . "local")
                (session_id . "session-1")
                (state . "idle")
                (viewer_attachment_state . "attached")
                (project . "scv")
                (title . "Build viewer")
                (last_activity . "2026-05-20T00:00:00Z")))
         (entry (scv-session-manager--entry row)))
    (should (equal (car entry) "local:session-1"))
    (should
     (equal (aref (cadr entry) 0) "local"))
    (should
     (equal (aref (cadr entry) 1) "session-1"))))

(ert-deftest scv-session-manager-refresh-reads-json-command ()
  (let* ((script
          (scv-test--write-script
           "printf '%s\\n' '[{\"origin\":\"local\",\"session_id\":\"session-1\",\"state\":\"idle\",\"viewer_attachment_state\":\"none\",\"project\":\"scv\",\"title\":\"Build viewer\",\"last_activity\":\"2026-05-20T00:00:00Z\"}]'\n"))
         (scv-executable script))
    (unwind-protect
        (with-temp-buffer
          (scv-session-manager-mode)
          (scv-session-manager-refresh)
          (should (= (length tabulated-list-entries) 1))
          (let ((entry (cadar tabulated-list-entries)))
            (should (equal (aref entry 1) "session-1"))
            (should (equal (aref entry 5) "Build viewer"))))
      (ignore-errors (delete-file script)))))

(ert-deftest scv-install-defaults-to-shift-s-prefix ()
  (let ((leader (make-sparse-keymap)))
    (scv-install leader)
    (should (keymapp (lookup-key leader (kbd "S"))))
    (should (eq (lookup-key leader (kbd "S m")) #'scv-menu))
    (should (eq (lookup-key leader (kbd "S n")) #'scv-new-session))
    (should-not (lookup-key leader (kbd "a")))))

(provide 'scv-tests)
;;; scv-tests.el ends here
