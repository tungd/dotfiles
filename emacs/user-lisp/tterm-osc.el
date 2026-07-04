;;; tterm-osc.el --- OSC and hyperlink handling for tterm -*- lexical-binding: t; -*-

;; Terminal OSC behavior and terminal link helpers.

(eval-and-compile
  (defconst tterm-osc--directory
    (file-name-directory (or load-file-name buffer-file-name default-directory))
    "Directory containing tterm OSC Lisp files."))

(require 'cl-lib)
(require 'subr-x)
(require 'thingatpt)
(require 'tterm-profiling (expand-file-name "tterm-profiling" tterm-osc--directory))

(declare-function tterm-dashboard "tterm-dashboard" ())
(declare-function tterm-cwd "tterm" (term))
(declare-function tterm-set-cwd "tterm" (term value))
(declare-function tterm--header-attention-indicator "tterm" ())
(declare-function tterm--shift-row "tterm-apply" (row))

(defvar tterm--terminal)
(defvar global-mode-string)

(defcustom tterm-buffer-title-function #'tterm-buffer-title-default
  "Function used to build tterm buffer names from current directory and title.
The function receives two arguments, CWD and TITLE, and should return the full
buffer name string."
  :type 'function
  :group 'tterm)

(defcustom tterm-osc-52-policy 'ask
  "Policy for OSC 52 clipboard writes.
`accept` writes to the kill ring automatically, `reject` ignores requests,
`confirm` prompts before writing, and `ignore` keeps clipboard writes silent."
  :type '(choice (const :tag "Accept" accept)
                 (const :tag "Reject" reject)
                 (const :tag "Confirm" confirm)
                 (const :tag "Ignore" ignore))
  :group 'tterm)

(defcustom tterm-osc-52-max-bytes 32768
  "Maximum decoded OSC 52 payload in bytes.
Larger payloads are ignored."
  :type 'integer
  :group 'tterm)

(defcustom tterm-notification-hook nil
  "Hook run for accepted terminal notifications.
Each function receives two arguments: the notification title and body.
The title may be nil for notification protocols that only provide a body."
  :type '(repeat function)
  :group 'tterm)

(defcustom tterm-notification-max-bytes 8192
  "Maximum terminal notification payload in bytes.
Larger payloads are ignored."
  :type 'integer
  :group 'tterm)

(defface tterm-notification-mode-line
  '((t :inherit mode-line-emphasis))
  "Face for tterm notification badges."
  :group 'tterm)

(defvar-local tterm--title nil
  "Latest terminal title for the current tterm buffer.")

(defvar-local tterm--osc-indicator nil
  "Current OSC indicator state.
One of `idle', `busy', `failed', or `unknown'.")

(defvar-local tterm--latest-notification nil
  "Latest OSC 9/777 notification summary for the current buffer.")

(defun tterm-osc-status (&optional buffer)
  "Return the latest OSC 133 status for BUFFER.
BUFFER defaults to the current buffer.  The result is one of `busy', `idle',
`failed', `unknown', or nil when no OSC 133 marker has been seen."
  (let ((target (or buffer (current-buffer))))
    (when (buffer-live-p target)
      (with-current-buffer target
        tterm--osc-indicator))))

(defun tterm-osc-status-string (&optional buffer)
  "Return a compact display string for BUFFER's OSC 133 status.
BUFFER defaults to the current buffer.  Return an empty string when no status is
available."
  (pcase (tterm-osc-status buffer)
    ('busy "busy")
    ('idle "idle")
    ('failed "failed")
    ('unknown "?")
    (_ "")))

(defvar-local tterm--active-hyperlink-id nil
  "Active hyperlink ID from the latest OSC 8 sequence.")

(defvar-local tterm--active-hyperlink-uri nil
  "Active hyperlink URI from the latest OSC 8 sequence.")

(defvar-local tterm--active-hyperlink-start nil
  "Buffer position where active hyperlink text was last started.")

(defconst tterm--hyperlink-property 'tterm-hyperlink-uri
  "Text property used to store hyperlink URI on terminal text.")

(defconst tterm--path-with-location-regexp
  "\\(?:~\\|\\.\\.?\\)?/[^[:space:]<>\"'`]+\\|\\.\\.?/[^[:space:]<>\"'`]+\\|[[:alnum:]_.@+-][[:alnum:]_.@+-]*/[^[:space:]<>\"'`]+"
  "Regexp matching terminal path text, possibly followed by line and column.")

(defun tterm--mode-line-osc-indicator ()
  "Return compact OSC indicator text for the mode line."
  (let ((status (tterm-osc-status-string)))
    (if (string-empty-p status)
        ""
      (format " [osc:%s]" status))))

(defun tterm--url-decode (value)
  "Decode percent-escaped octets in VALUE.
Malformed escapes are preserved."
  (let ((len (length value))
        (decoded "")
        (start 0))
    (while (< start len)
      (let ((c (aref value start)))
        (if (and (= c ?%)
                 (<= (+ start 2) (1- len))
                 (let ((h1 (aref value (+ start 1)))
                       (h2 (aref value (+ start 2))))
                   (and (or (and (>= h1 ?0) (<= h1 ?9))
                            (and (>= h1 ?A) (<= h1 ?F))
                            (and (>= h1 ?a) (<= h1 ?f)))
                        (or (and (>= h2 ?0) (<= h2 ?9))
                            (and (>= h2 ?A) (<= h2 ?F))
                            (and (>= h2 ?a) (<= h2 ?f))))))
            (let ((byte (string-to-number (substring value (+ start 1) (+ start 3)) 16)))
              (setq decoded (concat decoded (string byte)))
              (setq start (+ start 3)))
          (setq decoded (concat decoded (string c)))
          (setq start (1+ start)))))
    decoded))

(defun tterm--collapse-path-parents (path)
  "Return PATH with parent directories collapsed to initials.
The final path component is kept intact.  For example,
~/Projects/personal/tterm becomes ~/P/p/tterm."
  (let* ((abbrev (abbreviate-file-name (directory-file-name path)))
         (prefix "")
         (relative abbrev))
    (cond
     ((string-prefix-p "~/" abbrev)
      (setq prefix "~/"
            relative (substring abbrev 2)))
     ((string-prefix-p "/" abbrev)
      (setq prefix "/"
            relative (substring abbrev 1))))
    (let ((parts (split-string relative "/" t)))
      (cond
       ((null parts) prefix)
       ((null (cdr parts)) (concat prefix (car parts)))
       (t
        (concat
         prefix
         (mapconcat
          #'identity
          (append
           (mapcar (lambda (part) (substring part 0 1))
                   (butlast parts))
           (last parts))
          "/")))))))

(defun tterm-buffer-title-default (cwd title)
  "Return the default tterm buffer name for CWD and TITLE."
  (format "*tterm:%s*" (tterm--short-title cwd title)))

(defun tterm-buffer-title-collapse-parents (cwd title)
  "Return a tterm buffer name with collapsed parent directories.
For example, ~/Projects/personal/tterm becomes ~/P/p/tterm."
  (let ((cwd-str (if cwd (tterm--collapse-path-parents cwd) "")))
    (cond
     ((and (not (string-empty-p title)) (not (string-empty-p cwd-str)))
      (format "*tterm:%s: %s*" cwd-str title))
     ((not (string-empty-p cwd-str))
      (format "*tterm:%s*" cwd-str))
     ((not (string-empty-p title))
      (format "*tterm: %s*" title))
     (t (buffer-name)))))

(defun tterm--osc-cwd-path (data)
  "Return a local directory path from OSC 7 DATA."
  (let ((value (decode-coding-string data 'utf-8)))
    (when (string-prefix-p "file://" value)
      (setq value (substring value 7))
      (let ((slash (string-match-p "/" value)))
        (when slash
          (setq value (substring value slash)))))
    (tterm--url-decode value)))

(defun tterm--update-buffer-name ()
  "Update the current buffer's name based on current directory and window title."
  (when (buffer-name)
    (let* ((title (or (and (boundp 'tterm--title) tterm--title) ""))
           (cwd (or (and (boundp 'tterm--terminal)
                         tterm--terminal
                         (tterm-cwd tterm--terminal))
                    default-directory))
           (new-name (funcall tterm-buffer-title-function cwd title)))
      (unless (string= (buffer-name) new-name)
        (rename-buffer new-name t)))))

(defun tterm--short-title (cwd title)
  "Return compact tterm display title from CWD and TITLE."
  (cond
   ((and (stringp title) (not (string-empty-p title))) title)
   ((and (stringp cwd) (not (string-empty-p cwd)))
    (file-name-nondirectory (directory-file-name cwd)))
   (t "shell")))

(defun tterm-header-line-format ()
  "Return concise header-line text for the current tterm buffer."
  (when (and (boundp 'tterm--terminal) tterm--terminal)
    (let* ((handle (tterm-handle tterm--terminal))
           (host (or (and (listp handle) (plist-get handle :host))
                     (tterm-host tterm--terminal)
                     "local"))
           (session (or (and (listp handle) (plist-get handle :session)) ""))
           (cwd (or (tterm-cwd tterm--terminal) default-directory))
           (title (tterm--short-title cwd (or tterm--title
                                              (tterm-title tterm--terminal))))
           (status (tterm-osc-status-string))
           (notification tterm--latest-notification))
      (string-join
       (delq nil
             (list
              (if (string-empty-p session) host (format "%s:%s" host session))
              title
              (unless (string-empty-p status) (format "[%s]" status))
              (and cwd (abbreviate-file-name (directory-file-name cwd)))
              (and notification (format "notify: %s" notification))
              (and (fboundp 'tterm--header-attention-indicator)
                   (tterm--header-attention-indicator))))
       "  "))))

(defun tterm--set-cwd (data)
  "Update terminal current directory from OSC 7 DATA."
  (let* ((path (tterm--osc-cwd-path data))
         (directory (and (stringp path)
                         (not (string-empty-p path))
                         (file-name-as-directory (expand-file-name path)))))
    (when directory
      (when tterm--terminal
        (tterm-set-cwd tterm--terminal directory))
      (when (file-directory-p directory)
        (setq-local default-directory directory))
      (tterm--update-buffer-name))))

(defun tterm--osc-code-and-data (payload)
  "Return cons (CODE . DATA) from OSC PAYLOAD.
Supports standard one-byte OSC framing and a compatibility form where CODE is
the leading decimal digits followed by a semicolon."
  (when (> (length payload) 0)
    (let ((first (aref payload 0)))
      (if (and (>= first ?0) (<= first ?9))
          (let ((semi (string-match-p ";" payload)))
            (if semi
                (let ((code-str (substring payload 0 semi)))
                  (if (string-match-p "\\`[0-9]+\\'" code-str)
                      (cons (string-to-number code-str)
                            (substring payload (1+ semi)))
                    (cons first (substring payload 1))))
              (cons first (substring payload 1))))
        (cons first (substring payload 1))))))

(defun tterm--clipboard-policy-accepts-p ()
  "Return non-nil when current OSC 52 policy wants to write clipboard data."
  (pcase tterm-osc-52-policy
    ('accept t)
    ('confirm (y-or-n-p "Allow OSC 52 clipboard write? "))
    (_ nil)))

(defun tterm--set-terminal-clipboard (selection text)
  "Update kill ring and system clipboard with TEXT for SELECTION."
  (let ((payload (or text "")))
    (kill-new payload)
    (ignore-errors
      (pcase selection
        (1 (gui-set-selection 'CLIPBOARD payload))
        (0 (gui-set-selection 'PRIMARY payload))
        (_ nil)))
    payload))

(defun tterm--apply-hyperlink-properties (start end uri)
  "Apply terminal hyperlink properties from START to END for URI."
  (when (and (stringp uri)
             (not (string-empty-p uri))
             (> end start))
    (add-text-properties
     start end
     `(,tterm--hyperlink-property ,uri))))

(defun tterm--apply-active-hyperlink (start end)
  "Apply hyperlink text properties from START to END for active OSC 8 URI."
  (when (and tterm--active-hyperlink-uri
             (not (string-empty-p tterm--active-hyperlink-uri))
             (> end start))
    (tterm--apply-hyperlink-properties
     start end tterm--active-hyperlink-uri)))

(defun tterm--active-hyperlink-p ()
  "Return non-nil when newly inserted terminal text should inherit a hyperlink."
  (and tterm--active-hyperlink-uri
       (not (string-empty-p tterm--active-hyperlink-uri))))

(defun tterm--apply-hyperlink-span (row col len uri)
  "Apply hyperlink URI to ROW/COL covering LEN cells."
  (when (and (stringp uri) (not (string-empty-p uri)) (> len 0))
    (tterm--apply-profile-time :link-property-ms
      (save-excursion
        (goto-char (point-min))
        (forward-line (tterm--shift-row row))
        (move-to-column col)
        (let ((start (point)))
          (move-to-column (+ col len))
          (tterm--apply-hyperlink-properties start (point) uri))))))

(defun tterm--trim-link-end (text start end)
  "Return END trimmed for trailing punctuation in TEXT."
  (while (and (> end start)
              (memq (aref text (1- end)) '(?. ?, ?\; ?: ?! ?? ?\\ ?\" ?' ?\) ?\] ?})))
    (setq end (1- end)))
  end)

(defun tterm--local-file-from-uri (uri)
  "Return local file path represented by file URI, or nil."
  (when (string-prefix-p "file://" uri)
    (let ((value (substring uri 7)))
      (cond
       ((string-prefix-p "localhost/" value)
        (setq value (substring value 9)))
       ((string-prefix-p "/" value)
        nil)
       ((string-match-p "\\`[^/]+/" value)
        (setq value nil)))
      (when value
        (tterm--url-decode (concat "/" value))))))

(defun tterm--file-target (path)
  "Return an absolute existing file target for PATH, or nil."
  (let* ((expanded (expand-file-name (substitute-in-file-name path)))
         (candidate expanded))
    (while (and (not (file-exists-p candidate))
                (string-match "\\`\\(.+\\):[0-9]+\\(?::[0-9]+\\)?\\'" candidate))
      (setq candidate (match-string 1 candidate)))
    (when (file-exists-p candidate)
      candidate)))

(defun tterm--bounds-of-path-with-location-at-point ()
  "Return bounds of a terminal path with optional line and column at point."
  (let ((pos (point))
        (line-end (line-end-position))
        bounds)
    (save-excursion
      (goto-char (line-beginning-position))
      (while (and (not bounds)
                  (re-search-forward tterm--path-with-location-regexp line-end t))
        (let* ((start (match-beginning 0))
               (text (match-string-no-properties 0))
               (trimmed-end (+ start (tterm--trim-link-end text 0 (length text)))))
          (when (and (<= start pos)
                     (<= pos trimmed-end)
                     (not (string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*://"
                                          (buffer-substring-no-properties
                                           start trimmed-end))))
            (setq bounds (cons start trimmed-end))))))
    bounds))

(defun tterm--path-with-location-at-point ()
  "Return terminal path text at point, including optional line and column."
  (when-let* ((bounds (bounds-of-thing-at-point 'tterm-path-with-location)))
    (buffer-substring-no-properties (car bounds) (cdr bounds))))

(put 'tterm-path-with-location
     'bounds-of-thing-at-point
     #'tterm--bounds-of-path-with-location-at-point)
(put 'tterm-path-with-location
     'thing-at-point
     #'tterm--path-with-location-at-point)

(defun tterm--thing-at-point-target (&optional pos)
  "Return a URL or reachable local file target at POS using thing-at-point."
  (save-excursion
    (when pos
      (goto-char pos))
    (or (when-let* ((url (thing-at-point 'url t)))
          (let* ((trimmed (tterm--trim-link-end url 0 (length url)))
                 (target (substring url 0 trimmed)))
            (unless (string-empty-p target)
              target)))
        (when-let* ((path (or (thing-at-point 'tterm-path-with-location t)
                              (thing-at-point 'filename t)))
                    (target (tterm--file-target path)))
          target))))

(defun tterm-open-hyperlink-at-point (event)
  "Open terminal hyperlink at point or mouse EVENT position."
  (interactive "e")
  (let ((pos (posn-point (event-start event))))
    (if (and pos (not (= pos 0)))
        (tterm-open-hyperlink pos)
      (message "No hyperlink at point"))))

(defun tterm-open-hyperlink (&optional pos)
  "Open terminal hyperlink at point."
  (interactive)
  (let* ((position (or pos (point)))
         (target (or (tterm--hyperlink-uri-at-point position)
                     (tterm--thing-at-point-target position))))
    (if (not target)
        (message "No URL or file at point")
      (let ((file (or (tterm--local-file-from-uri target)
                      (and (file-exists-p target) target))))
        (cond
         (file (find-file file))
         ((fboundp 'browse-url) (browse-url target))
         (t (message "No browser helper for hyperlink: %s" target)))))))

(defun tterm--hyperlink-uri-at-point (&optional pos)
  "Return OSC 8 URI at POS, or at point."
  (let ((position (or pos (point))))
    (cond
     ((> position (point-max))
      nil)
     ((get-text-property position tterm--hyperlink-property)
      (get-text-property position tterm--hyperlink-property))
     ((and (> position (point-min))
           (get-text-property (1- position) tterm--hyperlink-property))
      (get-text-property (1- position) tterm--hyperlink-property))
     (t nil))))

(defun tterm--clear-active-hyperlink ()
  "Clear active OSC 8 hyperlink tracking state."
  (setq-local tterm--active-hyperlink-id nil
              tterm--active-hyperlink-uri nil
              tterm--active-hyperlink-start nil))

(defun tterm--parse-osc-8 (payload)
  "Parse OSC 8 payload string into (ID URI) list."
  (let ((semi (string-match-p ";" payload)))
    (when semi
      (let ((id (substring payload 0 semi))
            (uri (substring payload (1+ semi))))
        (cons id (if uri (tterm--url-decode uri) ""))))))

(defun tterm--handle-osc-8 (payload)
  "Handle OSC 8 hyperlink payload."
  (let ((link (tterm--parse-osc-8 payload)))
    (if (and link (not (string-empty-p (cdr link))))
        (setq-local tterm--active-hyperlink-id (car link)
                    tterm--active-hyperlink-uri (cdr link)
                    tterm--active-hyperlink-start nil)
      (tterm--clear-active-hyperlink))))

(defun tterm--handle-osc-52 (payload)
  "Handle OSC 52 clipboard payload."
  (let ((semi (string-match-p ";" payload)))
    (when (and semi (< 0 semi) (< semi (length payload)))
      (let* ((selection-token (substring payload 0 semi))
             (data-b64 (substring payload (1+ semi)))
             (selection (if (member selection-token '("c" "C")) 1 0)))
        (condition-case nil
            (let* ((raw-bytes (base64-decode-string data-b64 t t))
                   (raw-len (string-bytes raw-bytes))
                   (decoded (decode-coding-string raw-bytes 'utf-8 t)))
              (if (and (numberp raw-len)
                       (> raw-len tterm-osc-52-max-bytes))
                  (message "OSC52 payload too large (%d bytes)" raw-len)
                (when (and (stringp decoded)
                           (not (string-empty-p decoded))
                           (tterm--clipboard-policy-accepts-p))
                  (tterm--set-terminal-clipboard selection decoded))))
          (error
           (message "Malformed OSC52 payload ignored")))))))

(defun tterm--handle-osc-133 (payload)
  "Handle OSC 133 shell-integration markers."
  (let ((payload-text (or payload "")))
    (when (> (length payload-text) 0)
      (setq-local tterm--osc-indicator
                  (pcase (aref payload-text 0)
                    (?A 'busy)
                    (?B 'idle)
                    (?C 'failed)
                    (?P 'unknown)
                    (_ 'unknown)))
      (force-mode-line-update))))

(defun tterm--emit-notification (title body source)
  "Emit accepted terminal notification TITLE and BODY from SOURCE."
  (when (and (stringp body)
             (not (string-empty-p body))
             (<= (string-bytes body) tterm-notification-max-bytes))
    (setq-local tterm--latest-notification
                (if (and title (not (string-empty-p title)))
                    (format "%s: %s" title body)
                  body))
    (force-mode-line-update)
    (run-hook-with-args 'tterm-notification-hook title body)))

(defun tterm--handle-osc-9 (payload)
  "Handle OSC 9 desktop notification payload."
  (unless (or (string-empty-p payload)
              ;; OSC 9;4 is a progress sequence in iTerm2/ConEmu-style
              ;; protocols, not a desktop notification body.
              (string-match-p "\\`4\\(?:;\\|\\'\\)" payload))
    (tterm--emit-notification nil payload 'osc-9)))

(defun tterm--handle-osc-777 (payload)
  "Handle OSC 777 desktop notification payload."
  (when (string-match "\\`notify;\\([^;]+\\);\\(.+\\)\\'" payload)
    (let ((title (match-string 1 payload))
          (body (match-string 2 payload)))
      (when (<= (string-bytes payload) tterm-notification-max-bytes)
        (tterm--emit-notification title body 'osc-777)))))

(defun tterm--set-title (payload)
  "Set terminal title from PAYLOAD."
  (let ((title (decode-coding-string payload 'utf-8)))
    (setq-local tterm--title title)
    (tterm--update-buffer-name)))

(defun tterm--handle-osc (payload)
  "Handle OSC event from PAYLOAD."
  (let ((parsed (tterm--osc-code-and-data payload)))
    (when parsed
      (let ((osc-code (car parsed))
            (data (cdr parsed)))
        (pcase osc-code
          (0 (tterm--set-title data))
          (2 (tterm--set-title data))
          (7 (tterm--set-cwd data))
          (8 (tterm--handle-osc-8 data))
          (9 (tterm--handle-osc-9 data))
          (52 (tterm--handle-osc-52 data))
          (133 (tterm--handle-osc-133 data))
          (777 (tterm--handle-osc-777 data))
          (_ nil))))))

(defun tterm--handle-osc-op (op)
  "Handle an apply-ready OSC OP."
  (if (> (length op) 2)
      (let ((osc-code (aref op 1))
            (data (aref op 2)))
        (pcase osc-code
          (0 (tterm--set-title data))
          (2 (tterm--set-title data))
          (7 (tterm--set-cwd data))
          (8 (tterm--handle-osc-8 data))
          (9 (tterm--handle-osc-9 data))
          (52 (tterm--handle-osc-52 data))
          (133 (tterm--handle-osc-133 data))
          (777 (tterm--handle-osc-777 data))
          (_ nil)))
    (tterm--handle-osc (aref op 1))))

(provide 'tterm-osc)

;;; tterm-osc.el ends here
