;;; ob-http.el --- Local Org Babel HTTP client -*- lexical-binding: t; -*-

;; This is a local replacement for the unmaintained ob-http package.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'ob)
(require 'org)
(require 'org-element)
(require 'subr-x)

(defgroup td-ob-http nil
  "HTTP requests in Org Babel source blocks."
  :group 'org-babel)

(defcustom td-ob-http-curl-program "curl"
  "Curl executable used to execute HTTP source blocks."
  :type 'string
  :group 'td-ob-http)

(defcustom td-ob-http-max-time 180
  "Default maximum time in seconds for HTTP requests."
  :type 'integer
  :group 'td-ob-http)

(defcustom td-ob-http-default-response 'buffer
  "Default response shape inserted into Org results.

Valid values are `buffer', `body', `full', `headers', `status', and `raw'."
  :type '(choice (const buffer)
                 (const body)
                 (const full)
                 (const headers)
                 (const status)
                 (const raw))
  :group 'td-ob-http)

(defcustom td-ob-http-async-by-default t
  "Whether HTTP source blocks execute asynchronously by default.

Use the source block header argument `:async no' to force synchronous
execution for one block."
  :type 'boolean
  :group 'td-ob-http)

(defcustom td-ob-http-display-async-buffer-immediately t
  "Whether async buffer-shaped responses display their buffer immediately."
  :type 'boolean
  :group 'td-ob-http)

(defcustom td-ob-http-pretty-by-default t
  "Pretty-print JSON and XML response bodies by default."
  :type 'boolean
  :group 'td-ob-http)

(defcustom td-ob-http-curl-extra-arguments nil
  "Extra curl arguments added to every executed HTTP request."
  :type '(repeat string)
  :group 'td-ob-http)

(defcustom td-ob-http-postman-schema
  "https://schema.postman.com/collection/json/v2.1.0/draft-07/collection.json"
  "Postman collection schema URL used by export."
  :type 'string
  :group 'td-ob-http)

(defconst org-babel-header-args:http
  '((async . :any)
    (response . :any)
    (pretty . :any)
    (proxy . :any)
    (noproxy . :any)
    (curl . :any)
    (cookie . :any)
    (cookie-jar . :any)
    (schema . :any)
    (host . :any)
    (port . :any)
    (user . :any)
    (username . :any)
    (password . :any)
    (follow-redirect . :any)
    (path-prefix . :any)
    (resolve . :any)
    (max-time . :any)
    (get-header . :any)
    (select . :any)
    (file . :any))
  "Org Babel header arguments for HTTP source blocks.")

(cl-defstruct td-ob-http-request
  method url headers body params)

(cl-defstruct td-ob-http-response
  status headers body raw stderr exit-code command request)

(defvar td-ob-http-last-request nil
  "Most recent `td-ob-http-request' object.")

(defvar td-ob-http-last-response nil
  "Most recent `td-ob-http-response' object.")

(defvar td-ob-http--pending-async (make-hash-table :test 'equal)
  "Async HTTP executions keyed by execution id.")

(defvar td-ob-http--pending-blocks (make-hash-table :test 'equal)
  "Async HTTP executions keyed by source block identity.")

(defvar td-ob-http-mode-keywords
  (let* ((methods '("GET" "POST" "PUT" "PATCH" "DELETE" "OPTIONS" "HEAD"
                   "TRACE" "CONNECT"))
         (headers '("Accept" "Accept-Charset" "Accept-Encoding" "Accept-Language"
                    "Authorization" "Cache-Control" "Connection" "Content-Length"
                    "Content-MD5" "Content-Type" "Cookie" "Date" "Expect" "From"
                    "Host" "If-Match" "If-Modified-Since" "If-None-Match"
                    "If-Range" "If-Unmodified-Since" "Max-Forwards" "Origin"
                    "Pragma" "Proxy-Authorization" "Range" "Referer" "TE"
                    "User-Agent" "Upgrade" "Via" "Warning"))
         (method-regexp
          (rx-to-string
           `(seq bol (* blank)
                 (group (or ,@methods))
                 (+ blank)
                 (group (+ nonl))
                 eol)))
         (header-regexp
          (rx-to-string
           `(seq bol (* blank)
                 (group (or ,@headers))
                 ":"
                 (* blank)
                 (group (* nonl))
                 eol))))
    `((,method-regexp (1 font-lock-keyword-face)
                      (2 font-lock-function-name-face))
      (,header-regexp (1 font-lock-variable-name-face)
                      (2 font-lock-string-face))
      ("^\\s-*\\([^: \t\n]+\\):\\s-*\\(.*\\)$"
       (1 font-lock-variable-name-face)
       (2 font-lock-string-face))
      ("[?&=]\\|[{}\\[\\],:]" . font-lock-comment-face)))
  "Font-lock rules for `td-ob-http-mode'.")

(defvar td-ob-http-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k c") #'td-ob-http-copy-as-curl)
    (define-key map (kbd "C-c C-k f") #'td-ob-http-copy-as-fetch)
    (define-key map (kbd "C-c C-k p") #'td-ob-http-copy-as-python)
    map)
  "Keymap for `td-ob-http-mode'.")

(define-derived-mode td-ob-http-mode fundamental-mode "Org HTTP"
  "Major mode for editing Org Babel HTTP request bodies."
  (setq-local font-lock-defaults '(td-ob-http-mode-keywords)))

(defun td-ob-http--set-org-src-mode ()
  "Register `td-ob-http-mode' as the edit mode for HTTP source blocks."
  (setq org-src-lang-modes
        (cl-remove "http" org-src-lang-modes
                   :key #'car
                   :test #'string=))
  (add-to-list 'org-src-lang-modes '("http" . "td-ob-http")))

(defun td-ob-http--normalize-newlines (text)
  "Return TEXT with CRLF and CR newlines normalized to LF."
  (replace-regexp-in-string "\r\n?" "\n" (or text "") t t))

(defun td-ob-http--param (params key &optional default)
  "Return KEY from Babel PARAMS, or DEFAULT."
  (let ((value (cdr (assq key params))))
    (if (null value) default value)))

(defun td-ob-http--truthy-p (value)
  "Return non-nil when VALUE should count as true."
  (not (member (format "%s" value) '("" "nil" "no" "false" "0"))))

(defun td-ob-http--async-p (params)
  "Return non-nil when PARAMS request async execution."
  (td-ob-http--truthy-p
   (td-ob-http--param params :async
                      (if td-ob-http-async-by-default "yes" "no"))))

(defun td-ob-http--string-value (value)
  "Render VALUE as a source-block substitution string."
  (cond
   ((null value) "")
   ((stringp value) value)
   (t (format "%s" value))))

(defun td-ob-http--var-alist (params)
  "Return Babel variable assignments from PARAMS."
  (let (vars)
    (dolist (param params (nreverse vars))
      (when (eq (car-safe param) :var)
        (let ((var (cdr param)))
          (when (consp var)
            (push var vars)))))))

(defun td-ob-http-expand-variables (body params)
  "Expand ${name} variables in BODY from Babel PARAMS."
  (let ((result body)
        (vars (td-ob-http--var-alist params))
        (start 0))
    (while (string-match "\\${\\([^}]+\\)}" result start)
      (let* ((name (match-string 1 result))
             (symbol (intern-soft name))
             (value (or (cdr (assoc name vars))
                        (and symbol (cdr (assq symbol vars))))))
        (setq result
              (replace-match (td-ob-http--string-value value)
                             t t result))
        (setq start (match-beginning 0))))
    result))

(defun org-babel-expand-body:http (body params)
  "Expand an Org Babel HTTP source block BODY using PARAMS."
  (td-ob-http-expand-variables body params))

(defun td-ob-http--split-header-body (text)
  "Split request TEXT into header text and body text."
  (let ((text (td-ob-http--normalize-newlines text)))
    (if (string-match "\n[ \t]*\n" text)
        (list (substring text 0 (match-beginning 0))
              (substring text (match-end 0)))
      (list text nil))))

(defun td-ob-http--parse-header-line (line)
  "Parse one request header LINE."
  (when (string-match "\\`[ \t]*\\([^: \t][^:]*\\):[ \t]*\\(.*\\)\\'" line)
    (cons (string-trim (match-string 1 line))
          (match-string 2 line))))

(defun td-ob-http--parse-request-line (line)
  "Parse HTTP request LINE into (METHOD URL)."
  (unless (string-match "\\`[ \t]*\\([A-Za-z]+\\)[ \t]+\\(.+?\\)[ \t]*\\'" line)
    (user-error "Invalid HTTP request line: %s" line))
  (let* ((method (upcase (match-string 1 line)))
         (target (string-trim
                  (replace-regexp-in-string
                   "[ \t]+HTTP/[0-9.]+\\'" "" (match-string 2 line)))))
    (list method target)))

(defun td-ob-http--absolute-url (url params)
  "Return URL, resolving path-only URLs using PARAMS."
  (if (string-match-p "\\`[A-Za-z][A-Za-z0-9+.-]*://" url)
      url
    (let ((host (td-ob-http--param params :host))
          (schema (td-ob-http--param params :schema "http"))
          (port (td-ob-http--param params :port))
          (prefix (or (td-ob-http--param params :path-prefix) "")))
      (unless host
        (user-error "HTTP block uses a relative URL without :host"))
      (concat schema "://" host
              (if port (format ":%s" port) "")
              prefix
              (if (string-prefix-p "/" url) url (concat "/" url))))))

(defun td-ob-http-parse-request (input &optional params)
  "Parse HTTP request INPUT into a `td-ob-http-request'."
  (let* ((params (or params nil))
         (parts (td-ob-http--split-header-body (string-trim-left input)))
         (header-lines (split-string (car parts) "\n" t))
         (request-line (car header-lines))
         (method-url (td-ob-http--parse-request-line request-line))
         (headers (delq nil (mapcar #'td-ob-http--parse-header-line
                                     (cdr header-lines)))))
    (make-td-ob-http-request
     :method (car method-url)
     :url (td-ob-http--absolute-url (cadr method-url) params)
     :headers headers
     :body (cadr parts)
     :params params)))

(defun td-ob-http--header (headers name)
  "Return header NAME from HEADERS."
  (let ((downcased (downcase name))
        found)
    (dolist (header headers found)
      (when (string= (downcase (car header)) downcased)
        (setq found (cdr header))))))

(defun td-ob-http-request-content-type (request)
  "Return REQUEST content type, if present."
  (td-ob-http--header (td-ob-http-request-headers request) "content-type"))

(defun td-ob-http--content-language (content-type body)
  "Return a best-effort body language from CONTENT-TYPE and BODY."
  (cond
   ((and content-type (string-match-p "json" content-type)) "json")
   ((and content-type (string-match-p "\\(xml\\|html\\)" content-type)) "xml")
   ((and body (string-match-p "\\`[ \t\n]*[{\[]" body)) "json")
   ((and body (string-match-p "\\`[ \t\n]*<" body)) "xml")
   (t "text")))

(defun td-ob-http--flatten (items)
  "Flatten nested ITEMS, dropping nil values."
  (cond
   ((null items) nil)
   ((atom items) (list items))
   (t (append (td-ob-http--flatten (car items))
              (td-ob-http--flatten (cdr items))))))

(defun td-ob-http-curl-args (request &optional body-file)
  "Return curl args for REQUEST.

When BODY-FILE is non-nil, request body data is read from that file."
  (let* ((params (td-ob-http-request-params request))
         (method (td-ob-http-request-method request))
         (body (td-ob-http-request-body request)))
    (td-ob-http--flatten
     (append
      td-ob-http-curl-extra-arguments
      (list "--silent"
            "--show-error"
            "-i"
            (when (and (td-ob-http--param params :proxy)
                       (not (td-ob-http--param params :noproxy)))
              (list "-x" (td-ob-http--param params :proxy)))
            (when (td-ob-http--param params :noproxy)
              (list "--noproxy" "*"))
            (if (string= method "HEAD")
                "-I"
              (list "-X" method))
            (when (td-ob-http--truthy-p
                   (td-ob-http--param params :follow-redirect))
              "-L")
            (when (and (td-ob-http--param params :username)
                       (td-ob-http--param params :password))
              (list "--user"
                    (format "%s:%s"
                            (td-ob-http--param params :username)
                            (td-ob-http--param params :password))))
            (when (td-ob-http--param params :user)
              (list "--user" (td-ob-http--param params :user)))
            (mapcar (lambda (header)
                      (list "-H" (format "%s: %s" (car header) (cdr header))))
                    (td-ob-http-request-headers request))
            (when (and body (not (string-empty-p body)))
              (list "--data-binary"
                    (if body-file
                        (format "@%s" body-file)
                      body)))
            (when (td-ob-http--param params :cookie-jar)
              (list "--cookie-jar" (td-ob-http--param params :cookie-jar)))
            (when (td-ob-http--param params :cookie)
              (list "--cookie" (td-ob-http--param params :cookie)))
            (when (td-ob-http--param params :resolve)
              (mapcar (lambda (entry) (list "--resolve" entry))
                      (split-string (td-ob-http--param params :resolve)
                                    "," t "[ \t\n]+")))
            (when (td-ob-http--param params :curl)
              (split-string-and-unquote (td-ob-http--param params :curl)))
            "--max-time"
            (format "%s" (td-ob-http--param params :max-time
                                            td-ob-http-max-time))
            "--globoff"
            (td-ob-http-request-url request))))))

(defun td-ob-http-shell-command (program args)
  "Return shell command for PROGRAM and ARGS."
  (string-join (mapcar #'td-ob-http-shell-quote (cons program args)) " "))

(defun td-ob-http-shell-quote (arg)
  "Quote ARG as pasteable POSIX shell syntax."
  (let ((arg (format "%s" arg)))
    (if (string-empty-p arg)
        "''"
      (concat "'"
              (replace-regexp-in-string "'" "'\"'\"'" arg t t)
              "'"))))

(defun td-ob-http-render-curl (request)
  "Return REQUEST as a curl command."
  (td-ob-http-shell-command td-ob-http-curl-program
                            (td-ob-http-curl-args request)))

(defun td-ob-http--json-string (value)
  "Return VALUE encoded as a JSON string literal."
  (json-encode (or value "")))

(defun td-ob-http--render-js-object (pairs indent)
  "Render PAIRS as a JavaScript object literal using INDENT."
  (if pairs
      (concat "{\n"
              (string-join
               (mapcar (lambda (pair)
                         (format "%s  %s: %s"
                                 indent
                                 (td-ob-http--json-string (car pair))
                                 (td-ob-http--json-string (cdr pair))))
                       pairs)
               ",\n")
              "\n" indent "}")
    "{}"))

(defun td-ob-http-render-fetch (request)
  "Return REQUEST as a JavaScript fetch snippet."
  (let* ((body (td-ob-http-request-body request))
         (headers (td-ob-http-request-headers request)))
    (concat
     "const response = await fetch("
     (td-ob-http--json-string (td-ob-http-request-url request))
     ", {\n"
     "  method: "
     (td-ob-http--json-string (td-ob-http-request-method request))
     (if headers
         (concat ",\n  headers: "
                 (td-ob-http--render-js-object headers "  "))
       "")
     (if (and body (not (string-empty-p body)))
         (concat ",\n  body: " (td-ob-http--json-string body))
       "")
     "\n});\n"
     "const text = await response.text();")))

(defun td-ob-http-render-python (request)
  "Return REQUEST as a Python requests snippet."
  (let* ((body (td-ob-http-request-body request))
         (headers (td-ob-http-request-headers request)))
    (concat
     "import requests\n\n"
     "response = requests.request(\n"
     "    " (td-ob-http--json-string (td-ob-http-request-method request)) ",\n"
     "    " (td-ob-http--json-string (td-ob-http-request-url request))
     (if headers
         (concat ",\n    headers="
                 (td-ob-http--render-js-object headers "    "))
       "")
     (if (and body (not (string-empty-p body)))
         (concat ",\n    data=" (td-ob-http--json-string body))
       "")
     "\n)\n"
     "print(response.text)")))

(defun td-ob-http--response-block-at-start-p (text)
  "Return non-nil when TEXT starts with an HTTP response status line."
  (string-match-p "\\`HTTP/[0-9.]+[ \t]+[0-9][0-9][0-9]" text))

(defun td-ob-http-parse-response (raw &optional stderr exit-code command request)
  "Parse curl RAW output into a `td-ob-http-response'."
  (let ((text (td-ob-http--normalize-newlines raw))
        headers status body)
    (while (and (td-ob-http--response-block-at-start-p text)
                (string-match "\n\n" text))
      (let ((block (substring text 0 (match-beginning 0))))
        (setq text (substring text (match-end 0)))
        (setq headers (split-string block "\n" t))
        (when (and headers
                   (string-match "HTTP/[0-9.]+[ \t]+\\([0-9][0-9][0-9]\\)"
                                 (car headers)))
          (setq status (string-to-number (match-string 1 (car headers)))))))
    (setq body text)
    (make-td-ob-http-response
     :status status
     :headers headers
     :body body
     :raw raw
     :stderr stderr
     :exit-code exit-code
     :command command
     :request request)))

(defun td-ob-http--process-file (request args body-file)
  "Execute curl ARGS for REQUEST, using BODY-FILE when present."
  (let ((stderr-file (org-babel-temp-file "td-ob-http-stderr-"))
        exit-code stdout stderr)
    (with-temp-buffer
      (setq exit-code
            (apply #'call-process
                   td-ob-http-curl-program nil
                   (list t stderr-file) nil
                   args))
      (setq stdout (buffer-string)))
    (setq stderr
          (when (file-exists-p stderr-file)
            (with-temp-buffer
              (insert-file-contents-literally stderr-file)
              (buffer-string))))
    (when (and body-file (file-exists-p body-file))
      (delete-file body-file))
    (when (file-exists-p stderr-file)
      (delete-file stderr-file))
    (td-ob-http-parse-response
     stdout stderr exit-code
     (td-ob-http-shell-command td-ob-http-curl-program args)
     request)))

(defun td-ob-http-execute-request (request)
  "Execute REQUEST and return a `td-ob-http-response'."
  (let* ((body (td-ob-http-request-body request))
         (body-file (when (and body (not (string-empty-p body)))
                      (let ((file (org-babel-temp-file "td-ob-http-body-")))
                        (with-temp-file file
                          (insert body))
                        file)))
         (args (td-ob-http-curl-args request body-file)))
    (td-ob-http--process-file request args body-file)))

(defun td-ob-http--read-buffer (buffer)
  "Return BUFFER contents, or an empty string if BUFFER is dead."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (buffer-string))
    ""))

(defun td-ob-http--cleanup-async
    (exec-id block-key body-file stdout-buffer stderr-buffer marker)
  "Clean up async execution resources."
  (let ((entry (gethash exec-id td-ob-http--pending-async)))
    (remhash exec-id td-ob-http--pending-async)
    (when (and block-key (eq entry (gethash block-key td-ob-http--pending-blocks)))
      (remhash block-key td-ob-http--pending-blocks)))
  (when (and body-file (file-exists-p body-file))
    (delete-file body-file))
  (when (buffer-live-p stdout-buffer)
    (kill-buffer stdout-buffer))
  (when (buffer-live-p stderr-buffer)
    (kill-buffer stderr-buffer))
  (when (markerp marker)
    (set-marker marker nil)))

(defun td-ob-http--insert-async-result (marker result params info)
  "Insert async RESULT at MARKER using Babel PARAMS and INFO."
  (let ((buffer (marker-buffer marker))
        (result-params (cdr (assq :result-params params))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (save-restriction
            (widen)
            (goto-char marker)
            (org-babel-remove-result)
            (org-babel-insert-result result result-params info nil "http")))))))

(defun td-ob-http--append-buffer (buffer text)
  "Append TEXT to BUFFER when BUFFER is live."
  (when (and (bufferp buffer) (buffer-live-p buffer))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert text)))))

(defun td-ob-http--append-async-output (stdout-buffer response-buffer text)
  "Append async process TEXT to STDOUT-BUFFER and RESPONSE-BUFFER."
  (td-ob-http--append-buffer stdout-buffer text)
  (td-ob-http--append-buffer response-buffer text))

(defun td-ob-http--async-sentinel
    (process _event exec-id block-key request params info marker body-file
             stdout-buffer stderr-buffer command response-buffer)
  "Handle completion of async HTTP PROCESS."
  (when (memq (process-status process) '(exit signal))
    (let* ((exit-code (process-exit-status process))
           (stdout (td-ob-http--read-buffer stdout-buffer))
           (stderr (td-ob-http--read-buffer stderr-buffer))
           (response (td-ob-http-parse-response
                      stdout stderr exit-code command request))
           (entry (gethash exec-id td-ob-http--pending-async))
           (current-entry (and block-key
                               (gethash block-key td-ob-http--pending-blocks)))
           (current-p (and entry (eq entry current-entry)))
           (cancelled-p (or (process-get process 'td-ob-http-cancelled)
                            (and entry (plist-get entry :cancelled)))))
      (unwind-protect
          (cond
            ((and current-p (not cancelled-p))
             (setq td-ob-http-last-request request
                   td-ob-http-last-response response)
             (td-ob-http--insert-async-result
              marker
              (td-ob-http-format-response response params response-buffer)
              params info)
             (message "HTTP request %s finished with status %s"
                      exec-id exit-code))
           (cancelled-p nil)
           (t
            (message "HTTP request %s finished stale with status %s"
                     exec-id exit-code)))
        (td-ob-http--cleanup-async
         exec-id block-key body-file stdout-buffer stderr-buffer marker)))))

(defun td-ob-http--source-block-key ()
  "Return identity for the current Org source block."
  (let ((element (org-element-at-point)))
    (when (eq (org-element-type element) 'src-block)
      (list (current-buffer)
            (org-element-property :begin element)))))

(defun td-ob-http--pending-entry-process (entry)
  "Return process from pending ENTRY."
  (plist-get entry :process))

(defun td-ob-http--pending-entry-exec-id (entry)
  "Return exec id from pending ENTRY."
  (plist-get entry :exec-id))

(defun td-ob-http--cancel-entry
    (entry &optional reason keep-response-buffer)
  "Cancel pending async ENTRY for REASON."
  (when entry
    (let ((process (td-ob-http--pending-entry-process entry))
          (exec-id (td-ob-http--pending-entry-exec-id entry))
          (block-key (plist-get entry :block-key))
          (response-buffer (plist-get entry :response-buffer)))
      (plist-put entry :cancelled t)
      (when (processp process)
        (process-put process 'td-ob-http-cancelled t))
      (when (process-live-p process)
        (delete-process process))
      (unless keep-response-buffer
        (td-ob-http--mark-response-buffer-cancelled response-buffer reason))
      (remhash exec-id td-ob-http--pending-async)
      (when block-key
        (remhash block-key td-ob-http--pending-blocks))
      (message "HTTP request %s cancelled%s"
               exec-id
               (if reason (format ": %s" reason) "")))))

(defun td-ob-http--cancel-block (block-key &optional reason)
  "Cancel pending async request for BLOCK-KEY."
  (td-ob-http--cancel-entry
   (and block-key (gethash block-key td-ob-http--pending-blocks))
   reason))

(defun td-ob-http-execute-request-async (request params info)
  "Execute REQUEST asynchronously using PARAMS and source block INFO."
  (let* ((exec-id (format-time-string "%Y%m%d%H%M%S%6N"))
         (block-key (td-ob-http--source-block-key))
         (previous-entry (and block-key
                              (gethash block-key td-ob-http--pending-blocks)))
         (reuse-response-buffer (and previous-entry
                                     (plist-get previous-entry :response-buffer)))
         (body (td-ob-http-request-body request))
         (body-file (when (and body (not (string-empty-p body)))
                      (let ((file (org-babel-temp-file "td-ob-http-body-")))
                        (with-temp-file file
                          (insert body))
                        file)))
         (args (td-ob-http-curl-args request body-file))
         (stdout-buffer (generate-new-buffer
                         (format " *td-ob-http-stdout-%s*" exec-id)))
         (stderr-buffer (generate-new-buffer
                         (format " *td-ob-http-stderr-%s*" exec-id)))
         (marker (point-marker))
         (command (td-ob-http-shell-command td-ob-http-curl-program args))
         (response-buffer nil)
         process
         entry)
    (td-ob-http--cancel-entry previous-entry "replaced by newer run" t)
    (when (and td-ob-http-display-async-buffer-immediately
               (eq (td-ob-http--response-shape params) 'buffer))
      (setq response-buffer
            (td-ob-http--display-running-response-buffer
             request exec-id command reuse-response-buffer)))
    (setq process
          (make-process
           :name (format "td-ob-http-%s" exec-id)
           :buffer stdout-buffer
           :command (cons td-ob-http-curl-program args)
           :connection-type 'pipe
           :noquery t
           :filter
           (lambda (_proc text)
             (td-ob-http--append-async-output
              stdout-buffer response-buffer text))
           :stderr stderr-buffer
           :sentinel
           (lambda (proc event)
             (td-ob-http--async-sentinel
              proc event exec-id block-key request params info marker body-file
              stdout-buffer stderr-buffer command response-buffer))))
    (setq entry
          (list :exec-id exec-id
                :process process
                :buffer (current-buffer)
                :marker marker
                :block-key block-key
                :response-buffer response-buffer
                :request request))
    (puthash exec-id entry td-ob-http--pending-async)
    (when block-key
      (puthash block-key entry td-ob-http--pending-blocks))
    (format "HTTP request in progress\nExecution ID: %s"
            exec-id)))

(defun td-ob-http--source-block-key-at-point ()
  "Return current Org HTTP source block identity."
  (unless (derived-mode-p 'org-mode)
    (user-error "This command must run from an Org buffer"))
  (let ((info (org-babel-get-src-block-info)))
    (unless (string= (car info) "http")
      (user-error "Point is not in an http source block")))
  (td-ob-http--source-block-key))

(defun td-ob-http--header-lines-string (response)
  "Return RESPONSE headers as a string."
  (string-join (or (td-ob-http-response-headers response) '()) "\n"))

(defun td-ob-http--request-label (request)
  "Return a short label for REQUEST."
  (let* ((url (or (and request (td-ob-http-request-url request)) "request"))
         (without-scheme (replace-regexp-in-string
                          "\\`[A-Za-z][A-Za-z0-9+.-]*://" "" url))
         (method (or (and request (td-ob-http-request-method request)) "HTTP")))
    (format "%s %s"
            method
            (truncate-string-to-width without-scheme 48 nil nil "..."))))

(defun td-ob-http--response-buffer-base-name (request)
  "Return the preferred response buffer name for REQUEST."
  (format "*td-ob-http %s*" (td-ob-http--request-label request)))

(defun td-ob-http--response-buffer-name (response)
  "Return a fresh response buffer name for RESPONSE."
  (generate-new-buffer-name
   (td-ob-http--response-buffer-base-name
    (td-ob-http-response-request response))))

(defun td-ob-http--display-running-response-buffer
    (request exec-id command &optional reuse-buffer)
  "Display a running async buffer for REQUEST.

EXEC-ID is the async execution id and COMMAND is the rendered curl
command.  When REUSE-BUFFER is live, reuse it for the newer run."
  (let ((buffer (if (buffer-live-p reuse-buffer)
                    reuse-buffer
                  (generate-new-buffer
                   (td-ob-http--response-buffer-base-name request)))))
    (with-current-buffer buffer
      (when (bound-and-true-p view-mode)
        (view-mode -1))
      (let ((name (td-ob-http--response-buffer-base-name request)))
        (unless (string= (buffer-name) name)
          (rename-buffer name t)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Request: %s\n" (td-ob-http--request-label request)))
        (insert "Status: running\n")
        (insert (format "Execution ID: %s\n" exec-id))
        (insert "\nCommand:\n" command "\n")
        (insert "\nOutput:\n")
        (goto-char (point-min))))
    (display-buffer buffer)
    buffer))

(defun td-ob-http--mark-response-buffer-cancelled (buffer reason)
  "Mark live response BUFFER as cancelled with REASON."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (when (re-search-forward "^Status: running$" nil t)
          (replace-match "Status: cancelled" t t))
        (goto-char (point-max))
        (insert "\nCancelled"
                (if reason (format ": %s" reason) "")
                "\n")
        (view-mode 1)))))

(defun td-ob-http--display-response-buffer (response body &optional buffer)
  "Display RESPONSE in a separate buffer with BODY.

Return the buffer name."
  (let ((buffer (if (buffer-live-p buffer)
                    buffer
                  (get-buffer-create
                   (td-ob-http--response-buffer-name response)))))
    (with-current-buffer buffer
      (when (bound-and-true-p view-mode)
        (view-mode -1))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Request: %s\n"
                        (td-ob-http--request-label
                         (td-ob-http-response-request response))))
        (insert (format "Status: %s\n"
                        (or (td-ob-http-response-status response)
                            (td-ob-http-response-exit-code response)
                            "unknown")))
        (when-let* ((stderr (td-ob-http-response-stderr response))
                    ((not (string-empty-p (string-trim stderr)))))
          (insert "\nError:\n" stderr "\n"))
        (when-let* ((headers (td-ob-http--header-lines-string response))
                    ((not (string-empty-p headers))))
          (insert "\nHeaders:\n" headers "\n"))
        (insert "\nBody:\n" body)
        (goto-char (point-min))
        (view-mode 1)))
    (display-buffer buffer)
    (buffer-name buffer)))

(defun td-ob-http--buffer-result (response body &optional buffer)
  "Display RESPONSE BODY separately and return an Org result summary."
  (let ((buffer-name (td-ob-http--display-response-buffer
                      response body buffer))
        (status (or (td-ob-http-response-status response)
                    (td-ob-http-response-exit-code response)
                    "unknown")))
    (format "HTTP %s response in buffer: %s" status buffer-name)))

(defun td-ob-http--response-header-alist (response)
  "Return RESPONSE headers as an alist."
  (delq nil
        (mapcar (lambda (line)
                  (when (string-match "\\`\\([^:]+\\):[ \t]*\\(.*\\)\\'" line)
                    (cons (downcase (match-string 1 line))
                          (match-string 2 line))))
                (cdr (td-ob-http-response-headers response)))))

(defun td-ob-http-response-header (response name)
  "Return RESPONSE header NAME."
  (cdr (assoc (downcase name)
              (td-ob-http--response-header-alist response))))

(defun td-ob-http--pretty-json (text)
  "Pretty-print TEXT as JSON when possible."
  (condition-case nil
      (with-temp-buffer
        (insert text)
        (json-pretty-print-buffer)
        (buffer-string))
    (error text)))

(defun td-ob-http--pretty-xml (text)
  "Pretty-print TEXT as XML when xmllint is available."
  (if (executable-find "xmllint")
      (with-temp-buffer
        (insert text)
        (if (zerop (call-process-region
                    (point-min) (point-max) "xmllint" t t nil
                    "--format" "-"))
            (buffer-string)
          text))
    text))

(defun td-ob-http--pretty-body (response pretty)
  "Return RESPONSE body, applying PRETTY policy."
  (let* ((body (td-ob-http-response-body response))
         (content-type (td-ob-http-response-header response "content-type"))
         (language (td-ob-http--content-language content-type body)))
    (pcase (if (eq pretty t) language (format "%s" pretty))
      ((or "no" "nil" "") body)
      ((or "yes" "auto")
       (pcase language
         ("json" (td-ob-http--pretty-json body))
         ("xml" (td-ob-http--pretty-xml body))
         (_ body)))
      ("json" (td-ob-http--pretty-json body))
      ("xml" (td-ob-http--pretty-xml body))
      (_ body))))

(defun td-ob-http--response-shape (params)
  "Return desired response shape from PARAMS."
  (let ((value (or (td-ob-http--param params :response)
                   td-ob-http-default-response)))
    (if (symbolp value) value (intern (format "%s" value)))))

(defun td-ob-http-format-response (response params &optional response-buffer)
  "Format RESPONSE according to Babel PARAMS."
  (let* ((exit-code (or (td-ob-http-response-exit-code response) 0))
         (stderr (string-trim (or (td-ob-http-response-stderr response) "")))
         (pretty-param (td-ob-http--param params :pretty
                                          (and td-ob-http-pretty-by-default
                                               'auto)))
         (body (td-ob-http--pretty-body response pretty-param)))
    (if (not (zerop exit-code))
        (string-join
         (delq nil
               (list (format "curl exited with status %s" exit-code)
                     (unless (string-empty-p stderr) stderr)
                     (unless (string-empty-p body) body)))
         "\n\n")
      (cond
       ((td-ob-http--param params :get-header)
        (or (td-ob-http-response-header
             response (td-ob-http--param params :get-header))
            ""))
       ((td-ob-http--param params :file)
        (let ((file (td-ob-http--param params :file)))
          (with-temp-file file
            (insert body))
          file))
       ((td-ob-http--param params :select)
        (td-ob-http-select response (td-ob-http--param params :select)))
       (t
        (pcase (td-ob-http--response-shape params)
          ('buffer (td-ob-http--buffer-result response body response-buffer))
          ('body body)
          ('full (string-join (list (td-ob-http--header-lines-string response)
                                    body)
                              "\n\n"))
          ('headers (td-ob-http--header-lines-string response))
          ('status (format "%s" (or (td-ob-http-response-status response)
                                    exit-code)))
          ('raw (td-ob-http-response-raw response))
          (_ body)))))))

(defun td-ob-http-select (response selector)
  "Select data from RESPONSE body using SELECTOR and jq when possible."
  (let ((body (td-ob-http-response-body response)))
    (if (and (executable-find "jq")
             (string-match-p "json" (or (td-ob-http-response-header
                                         response "content-type")
                                        "")))
        (with-temp-buffer
          (insert body)
          (if (zerop (call-process-region
                      (point-min) (point-max) "jq" t t nil "-r" selector))
              (buffer-string)
            body))
      body)))

(defun org-babel-execute:http (body params)
  "Execute an Org Babel HTTP source block."
  (let* ((expanded (org-babel-expand-body:http body params))
         (request (td-ob-http-parse-request expanded params))
         (info (org-babel-get-src-block-info)))
    (setq td-ob-http-last-request request
          td-ob-http-last-response nil)
    (if (td-ob-http--async-p params)
        (td-ob-http-execute-request-async request params info)
      (let ((response (td-ob-http-execute-request request)))
        (setq td-ob-http-last-response response)
        (td-ob-http-format-response response params)))))

;;;###autoload
(defun td-ob-http-cancel-at-point ()
  "Cancel the in-flight HTTP request for the Org source block at point."
  (interactive)
  (let ((block-key (td-ob-http--source-block-key-at-point)))
    (unless (gethash block-key td-ob-http--pending-blocks)
      (user-error "No in-flight HTTP request for this block"))
    (td-ob-http--cancel-block block-key "cancelled by user")))

(defun td-ob-http--request-at-point ()
  "Return parsed HTTP request for the Org source block at point."
  (cond
   ((derived-mode-p 'org-mode)
    (let* ((info (org-babel-get-src-block-info))
           (lang (car info))
           (body (nth 1 info))
           (params (nth 2 info)))
      (unless (string= lang "http")
        (user-error "Point is not in an http source block"))
      (td-ob-http-parse-request
       (org-babel-expand-body:http body params)
       params)))
   ((derived-mode-p 'td-ob-http-mode)
    (td-ob-http-parse-request
     (buffer-substring-no-properties (point-min) (point-max))
     nil))
   (t
    (user-error "This command must run from an Org http block or td-ob-http-mode"))))

(defun td-ob-http--copy (text label)
  "Copy TEXT to kill ring and report LABEL."
  (kill-new text)
  (message "Copied %s" label)
  text)

;;;###autoload
(defun td-ob-http-copy-as-curl ()
  "Copy current Org HTTP source block as a curl command."
  (interactive)
  (td-ob-http--copy
   (td-ob-http-render-curl (td-ob-http--request-at-point))
   "curl command"))

;;;###autoload
(defun td-ob-http-copy-as-fetch ()
  "Copy current Org HTTP source block as a JavaScript fetch snippet."
  (interactive)
  (td-ob-http--copy
   (td-ob-http-render-fetch (td-ob-http--request-at-point))
   "fetch snippet"))

;;;###autoload
(defun td-ob-http-copy-as-python ()
  "Copy current Org HTTP source block as a Python requests snippet."
  (interactive)
  (td-ob-http--copy
   (td-ob-http-render-python (td-ob-http--request-at-point))
   "Python snippet"))

(defun td-ob-http--postman-body-language (request)
  "Return Postman raw body language for REQUEST."
  (td-ob-http--content-language
   (td-ob-http-request-content-type request)
   (td-ob-http-request-body request)))

(defun td-ob-http--postman-header (header)
  "Return Postman representation for HEADER."
  `((key . ,(car header))
    (value . ,(cdr header))))

(defun td-ob-http-request-to-postman-item (request name)
  "Convert REQUEST to a Postman item named NAME."
  (let* ((body (td-ob-http-request-body request))
         (language (td-ob-http--postman-body-language request))
         (request-object
          `((method . ,(td-ob-http-request-method request))
            (header . ,(vconcat
                        (mapcar #'td-ob-http--postman-header
                                (td-ob-http-request-headers request))))
            (url . ((raw . ,(td-ob-http-request-url request)))))))
    (when (and body (not (string-empty-p body)))
      (setq request-object
            (append request-object
                    `((body . ((mode . "raw")
                               (raw . ,body)
                               (options . ((raw . ((language . ,language)))))))))))
    `((name . ,name)
      (request . ,request-object))))

(defun td-ob-http--src-block-name (block)
  "Return a stable request name for Org src BLOCK."
  (or (org-element-property :name block)
      (when-let* ((headline (org-element-lineage block '(headline) t)))
        (org-element-property :raw-value headline))
      "HTTP request"))

(defun td-ob-http--src-block-to-postman-item (block)
  "Convert Org src BLOCK to a Postman item, if it is an HTTP block."
  (when (string= (org-element-property :language block) "http")
    (let* ((body (org-element-property :value block))
           (params (org-babel-parse-header-arguments
                    (or (org-element-property :parameters block) "")))
           (request (td-ob-http-parse-request
                     (org-babel-expand-body:http body params)
                     params))
           (name (td-ob-http--src-block-name block)))
      (td-ob-http-request-to-postman-item request name))))

(defun td-ob-http-buffer-postman-collection (&optional name)
  "Return current Org buffer as a Postman collection named NAME."
  (let ((items (delq nil
                     (org-element-map (org-element-parse-buffer) 'src-block
                       #'td-ob-http--src-block-to-postman-item))))
    `((info . ((name . ,(or name
                            (file-name-base (or buffer-file-name
                                                (buffer-name)))))
               (schema . ,td-ob-http-postman-schema)))
      (item . ,(vconcat items)))))

(defun td-ob-http--json-pretty-string (object)
  "Return OBJECT encoded as pretty JSON."
  (with-temp-buffer
    (insert (json-encode object))
    (json-pretty-print-buffer)
    (buffer-string)))

;;;###autoload
(defun td-ob-http-export-postman-collection (file &optional name)
  "Export HTTP source blocks in the current Org buffer to Postman FILE.

With prefix NAME, prompt for the collection name."
  (interactive
   (list (read-file-name "Export Postman collection: " nil nil nil
                         (concat (file-name-base (or buffer-file-name
                                                     (buffer-name)))
                                 ".postman_collection.json"))
         (when current-prefix-arg
           (read-string "Collection name: "
                        (file-name-base (or buffer-file-name
                                            (buffer-name)))))))
  (unless (derived-mode-p 'org-mode)
    (user-error "This command must run from an Org buffer"))
  (let ((collection (td-ob-http-buffer-postman-collection name)))
    (with-temp-file file
      (insert (td-ob-http--json-pretty-string collection)))
    (message "Exported Postman collection to %s" file)
    file))

(defun td-ob-http--alist-get (key alist)
  "Return KEY from ALIST."
  (alist-get key alist nil nil #'eq))

(defun td-ob-http--postman-url-raw (url)
  "Return raw URL string from Postman URL value URL."
  (cond
   ((stringp url) url)
   ((and (listp url) (td-ob-http--alist-get 'raw url)))
   (t "")))

(defun td-ob-http--postman-headers (headers)
  "Return Org header lines for Postman HEADERS."
  (mapconcat
   (lambda (header)
     (format "%s: %s"
             (or (td-ob-http--alist-get 'key header) "")
             (or (td-ob-http--alist-get 'value header) "")))
   headers
   "\n"))

(defun td-ob-http--postman-request-source (request)
  "Return Org source body for a Postman REQUEST."
  (let* ((method (or (td-ob-http--alist-get 'method request) "GET"))
         (url (td-ob-http--postman-url-raw
               (td-ob-http--alist-get 'url request)))
         (headers (td-ob-http--postman-headers
                   (or (td-ob-http--alist-get 'header request) '())))
         (body-object (td-ob-http--alist-get 'body request))
         (body (and (listp body-object)
                    (td-ob-http--alist-get 'raw body-object))))
    (concat method " " url
            (unless (string-empty-p headers)
              (concat "\n" headers))
            (when (and body (not (string-empty-p body)))
              (concat "\n\n" body)))))

(defun td-ob-http--insert-postman-items (items level)
  "Insert Postman ITEMS as Org headings at LEVEL."
  (dolist (item items)
    (let ((name (or (td-ob-http--alist-get 'name item) "Request"))
          (request (td-ob-http--alist-get 'request item))
          (children (td-ob-http--alist-get 'item item)))
      (insert (make-string level ?*) " " name "\n\n")
      (if request
          (insert "#+begin_src http\n"
                  (td-ob-http--postman-request-source request)
                  "\n#+end_src\n\n")
        (td-ob-http--insert-postman-items children (1+ level))))))

;;;###autoload
(defun td-ob-http-import-postman-collection (file)
  "Insert Postman collection FILE into the current Org buffer."
  (interactive "fImport Postman collection: ")
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (collection (json-read-file file))
         (items (td-ob-http--alist-get 'item collection)))
    (td-ob-http--insert-postman-items items 1)))

(td-ob-http--set-org-src-mode)

(provide 'ob-http)

;;; ob-http.el ends here
