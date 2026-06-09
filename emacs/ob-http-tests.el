;;; ob-http-tests.el --- Tests for local ob-http -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)

(setq load-prefer-newer t)

(add-to-list 'load-path
             (expand-file-name "user-lisp"
                               (file-name-directory
                                (or load-file-name buffer-file-name))))
(require 'ob-http)

(defun td-ob-http-test--fake-curl (body &optional delay)
  "Create a fake curl executable that prints BODY after optional DELAY."
  (let ((file (make-temp-file "td-ob-http-fake-curl-" nil ".sh")))
    (with-temp-file file
      (insert "#!/bin/sh\n")
      (when delay
        (insert (format "sleep %s\n" delay)))
      (insert "cat <<'EOF'\n")
      (insert body)
      (insert "\nEOF\n"))
    (set-file-modes file #o755)
    file))

(defun td-ob-http-test--fake-curl-by-url ()
  "Create a fake curl executable that responds based on the URL argument."
  (let ((file (make-temp-file "td-ob-http-fake-curl-" nil ".sh")))
    (with-temp-file file
      (insert "#!/bin/sh\n")
      (insert "url=\"${@: -1}\"\n")
      (insert "case \"$url\" in\n")
      (insert "  *slow*) sleep 5; body='{\"run\":\"slow\"}' ;;\n")
      (insert "  *) body='{\"run\":\"fast\"}' ;;\n")
      (insert "esac\n")
      (insert "printf 'HTTP/1.1 200 OK\\r\\nContent-Type: application/json\\r\\n\\r\\n%s\\n' \"$body\"\n"))
    (set-file-modes file #o755)
    file))

(defun td-ob-http-test--kill-response-buffers ()
  "Kill response buffers created by ob-http tests."
  (dolist (buffer (buffer-list))
    (when (string-prefix-p "*td-ob-http " (buffer-name buffer))
      (kill-buffer buffer))))

(ert-deftest td-ob-http-parses-indented-request-with-body ()
  (let* ((request
          (td-ob-http-parse-request
           "  POST https://api.example.test/v1/chat\n  Authorization: Bearer token\n  Content-Type: application/json\n\n  {\"hello\":\"world\"}"
           nil)))
    (should (equal (td-ob-http-request-method request) "POST"))
    (should (equal (td-ob-http-request-url request)
                   "https://api.example.test/v1/chat"))
    (should (equal (td-ob-http-request-content-type request)
                   "application/json"))
    (should (equal (td-ob-http-request-body request)
                   "  {\"hello\":\"world\"}"))))

(ert-deftest td-ob-http-renders-curl-with-sensitive-headers ()
  (let* ((request
          (td-ob-http-parse-request
           "GET https://api.example.test\nAuthorization: Bearer secret\nX-Api-Key: key-1"
           nil))
         (curl (td-ob-http-render-curl request)))
    (should (string-match-p "secret" curl))
    (should (string-match-p "key-1" curl))
    (should-not (string-match-p "REDACTED" curl))))

(ert-deftest td-ob-http-renders-pasteable-curl-with-multiline-json ()
  (let* ((request
          (td-ob-http-parse-request
           "POST https://api.example.test\nContent-Type: application/json\n\n{\"message\":\"What is CloudFlare?\"}\n"
           nil))
         (curl (td-ob-http-render-curl request)))
    (should (string-match-p "'--data-binary' '{\"message\"" curl))
    (should (string-match-p "CloudFlare\\?\"}" curl))
    (should-not (string-match-p "\\\\:" curl))
    (should-not (string-match-p "'\n'" curl))))

(ert-deftest td-ob-http-sync-execution-displays-buffer-by-default ()
  (td-ob-http-test--kill-response-buffers)
  (let ((td-ob-http-curl-program
         (td-ob-http-test--fake-curl
          "HTTP/1.1 200 OK\r
Content-Type: application/json\r
\r
{\"ok\":true}"))
        (td-ob-http-default-response 'buffer)
        (org-confirm-babel-evaluate nil))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "#+begin_src http :async no\nGET https://api.example.test\n#+end_src\n")
          (goto-char (point-min))
          (search-forward "#+begin_src")
          (org-babel-execute-src-block)
          (should (string-match-p "HTTP 200 response in buffer:"
                                  (buffer-string)))
          (should-not (string-match-p "\"ok\": true" (buffer-string)))
          (should
           (cl-some (lambda (buffer)
                      (and (string-prefix-p "*td-ob-http "
                                            (buffer-name buffer))
                           (with-current-buffer buffer
                             (string-match-p "\"ok\": true"
                                             (buffer-string)))))
                    (buffer-list))))
      (delete-file td-ob-http-curl-program)
      (td-ob-http-test--kill-response-buffers))))

(ert-deftest td-ob-http-sync-execution-inserts-body-when-requested ()
  (let ((td-ob-http-curl-program
         (td-ob-http-test--fake-curl
          "HTTP/1.1 200 OK\r
Content-Type: application/json\r
\r
{\"ok\":true}"))
        (td-ob-http-default-response 'buffer)
        (org-confirm-babel-evaluate nil))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "#+begin_src http :async no :response body\nGET https://api.example.test\n#+end_src\n")
          (goto-char (point-min))
          (search-forward "#+begin_src")
          (org-babel-execute-src-block)
          (should (string-match-p "\"ok\": true" (buffer-string)))
          (should-not (string-match-p "HTTP request in progress" (buffer-string))))
      (delete-file td-ob-http-curl-program))))

(ert-deftest td-ob-http-async-execution-replaces-placeholder ()
  (let ((td-ob-http-curl-program
        (td-ob-http-test--fake-curl
          "HTTP/1.1 200 OK\r
Content-Type: application/json\r
\r
{\"async\":true}"
          "0.2"))
        (td-ob-http-async-by-default t)
        (td-ob-http-display-async-buffer-immediately t)
        (td-ob-http-default-response 'buffer)
        (org-confirm-babel-evaluate nil))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "#+begin_src http\nGET https://api.example.test\n#+end_src\n")
          (goto-char (point-min))
          (search-forward "#+begin_src")
          (org-babel-execute-src-block)
          (should (string-match-p "HTTP request in progress" (buffer-string)))
          (should-not (string-match-p "curl --silent" (buffer-string)))
          (let ((process-buffer
                 (cl-find-if
                  (lambda (buffer)
                    (and (string-prefix-p "*td-ob-http "
                                          (buffer-name buffer))
                         (with-current-buffer buffer
                           (string-match-p "Status: running"
                                           (buffer-string)))))
                  (buffer-list))))
            (should process-buffer)
            (with-current-buffer process-buffer
              (should (string-match-p "Execution ID:" (buffer-string)))
              (should (string-match-p "\nOutput:\n" (buffer-string)))))
          (let ((deadline (+ (float-time) 2.0)))
            (while (and (> deadline (float-time))
                        (string-match-p "HTTP request in progress"
                                        (buffer-string)))
              (accept-process-output nil 0.05)))
          (should (string-match-p "HTTP 200 response in buffer:"
                                  (buffer-string)))
          (should-not (string-match-p "\"async\": true" (buffer-string)))
          (should
           (cl-some (lambda (buffer)
                      (and (string-prefix-p "*td-ob-http "
                                            (buffer-name buffer))
                           (with-current-buffer buffer
                             (string-match-p "\"async\": true"
                                             (buffer-string)))))
                    (buffer-list)))
          (should-not (string-match-p "HTTP request in progress"
                                      (buffer-string))))
      (delete-file td-ob-http-curl-program)
      (td-ob-http-test--kill-response-buffers))))

(ert-deftest td-ob-http-rerun-cancels-in-flight-request ()
  (td-ob-http-test--kill-response-buffers)
  (clrhash td-ob-http--pending-async)
  (clrhash td-ob-http--pending-blocks)
  (let ((td-ob-http-curl-program (td-ob-http-test--fake-curl-by-url))
        (td-ob-http-async-by-default t)
        (td-ob-http-default-response 'body)
        (org-confirm-babel-evaluate nil))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "#+begin_src http\nGET https://api.example.test/slow\n#+end_src\n")
          (goto-char (point-min))
          (search-forward "#+begin_src")
          (org-babel-execute-src-block)
          (should (= (hash-table-count td-ob-http--pending-async) 1))
          (should (= (hash-table-count td-ob-http--pending-blocks) 1))
          (goto-char (point-min))
          (search-forward "slow")
          (replace-match "fast")
          (goto-char (point-min))
          (search-forward "#+begin_src")
          (org-babel-execute-src-block)
          (should (= (hash-table-count td-ob-http--pending-async) 1))
          (should (= (hash-table-count td-ob-http--pending-blocks) 1))
          (let ((deadline (+ (float-time) 2.0)))
            (while (and (> deadline (float-time))
                        (not (string-match-p "\"run\": \"fast\""
                                             (buffer-string))))
              (accept-process-output nil 0.05)))
          (should (string-match-p "\"run\": \"fast\"" (buffer-string)))
          (should-not (string-match-p "\"run\": \"slow\"" (buffer-string)))
          (should (= (hash-table-count td-ob-http--pending-async) 0))
          (should (= (hash-table-count td-ob-http--pending-blocks) 0)))
      (delete-file td-ob-http-curl-program)
      (td-ob-http-test--kill-response-buffers)
      (clrhash td-ob-http--pending-async)
      (clrhash td-ob-http--pending-blocks))))

(ert-deftest td-ob-http-exports-postman-collection ()
  (with-temp-buffer
    (org-mode)
    (insert "* Create thing\n\n")
    (insert "#+begin_src http\n")
    (insert "POST https://api.example.test/things\n")
    (insert "Content-Type: application/json\n\n")
    (insert "{\"name\":\"thing\"}\n")
    (insert "#+end_src\n")
    (let* ((collection (td-ob-http-buffer-postman-collection "Example"))
           (info (alist-get 'info collection))
           (items (append (alist-get 'item collection) nil))
           (item (car items))
           (request (alist-get 'request item)))
      (should (equal (alist-get 'name info) "Example"))
      (should (equal (alist-get 'name item) "Create thing"))
      (should (equal (alist-get 'method request) "POST"))
      (should (equal (alist-get 'raw (alist-get 'url request))
                     "https://api.example.test/things"))
      (should (equal (alist-get 'raw (alist-get 'body request))
                     "{\"name\":\"thing\"}\n")))))

(ert-deftest td-ob-http-imports-postman-collection ()
  (let ((file (make-temp-file "td-ob-http-postman-" nil ".json")))
    (unwind-protect
        (progn
          (let ((collection
                 `((info . ((name . "Example")))
                   (item . ,(vector
                             `((name . "Fetch thing")
                               (request . ((method . "GET")
                                           (header . ,(vector
                                                       '((key . "Accept")
                                                         (value . "application/json"))))
                                           (url . ((raw . "https://api.example.test/things/1")))))))))))
            (with-temp-file file
              (insert (json-encode collection))))
          (with-temp-buffer
            (td-ob-http-import-postman-collection file)
            (should (string-match-p "^\\* Fetch thing" (buffer-string)))
            (should (string-match-p "GET https://api.example.test/things/1"
                                    (buffer-string)))
            (should (string-match-p "Accept: application/json"
                                    (buffer-string)))))
      (delete-file file))))

(provide 'ob-http-tests)

;;; ob-http-tests.el ends here
