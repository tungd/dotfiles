;;; quota-dashboard.el --- Local AI quota dashboard -*- lexical-binding: t; -*-

;;; Commentary:
;; Render the snapshot produced by the local Quota macOS app as a compact,
;; read-only Emacs dashboard.  This file reads one local JSON file; it does not
;; contact providers and never needs their credentials.

;;; Code:

(require 'json)
(require 'subr-x)

(defgroup quota-dashboard nil
  "Local AI quota dashboard."
  :group 'tools)

(defcustom quota-dashboard-snapshot-file
  (expand-file-name
   "~/Library/Group Containers/P2FJWTSN96.group.com.tung.aiquotawidget/AIQuotaWidget/snapshot.json")
  "Snapshot written by the local Quota macOS app."
  :type 'file
  :group 'quota-dashboard)

(defcustom quota-dashboard-fallback-snapshot-file
  (expand-file-name "~/Library/Application Support/AIQuotaWidget/snapshot.json")
  "Legacy snapshot path used by older local Quota builds."
  :type 'file
  :group 'quota-dashboard)

(defcustom quota-dashboard-refresh-interval 30.0
  "Seconds between automatic dashboard refreshes."
  :type 'number
  :group 'quota-dashboard)

(defcustom quota-dashboard-bar-width 24
  "Number of cells used for each quota bar."
  :type 'integer
  :group 'quota-dashboard)

(defface quota-dashboard-title
  '((t (:inherit mode-line-buffer-id :weight bold :height 1.2)))
  "Face for the dashboard title."
  :group 'quota-dashboard)

(defface quota-dashboard-provider
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Face for provider names."
  :group 'quota-dashboard)

(defface quota-dashboard-bar-filled
  '((t (:inherit font-lock-constant-face)))
  "Face for the used part of a quota bar."
  :group 'quota-dashboard)

(defface quota-dashboard-bar-warning
  '((t (:inherit warning)))
  "Face for a quota bar close to its limit."
  :group 'quota-dashboard)

(defface quota-dashboard-bar-empty
  '((t (:inherit shadow)))
  "Face for the unused part of a quota bar."
  :group 'quota-dashboard)

(defface quota-dashboard-meta
  '((t (:inherit shadow)))
  "Face for dashboard metadata."
  :group 'quota-dashboard)

(defface quota-dashboard-ahead
  '((t (:inherit warning)))
  "Face for usage ahead of elapsed-window pace."
  :group 'quota-dashboard)

(defface quota-dashboard-behind
  '((t (:inherit success)))
  "Face for usage behind elapsed-window pace."
  :group 'quota-dashboard)

(defvar quota-dashboard-buffer-name "*Quota*"
  "Buffer name used for the Quota dashboard.")

(defvar-local quota-dashboard--refresh-timer nil
  "Buffer-local timer for refreshing the Quota dashboard.")

(defvar quota-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    map)
  "Keymap for `quota-dashboard-mode'.")

(define-key quota-dashboard-mode-map (kbd "g") #'quota-dashboard-refresh)
(define-key quota-dashboard-mode-map (kbd "r") #'quota-dashboard-refresh)

(defun quota-dashboard--get (key object)
  "Return KEY from JSON alist OBJECT."
  (alist-get key object))

(defun quota-dashboard--number (value)
  "Return VALUE as a number, or nil when it is not numeric."
  (when (numberp value)
    (float value)))

(defun quota-dashboard--parse-time (value)
  "Parse an ISO-8601 VALUE into a floating-point Unix timestamp."
  (when (stringp value)
    (condition-case nil
        (float-time (date-to-time value))
      (error nil))))

(defun quota-dashboard--duration (seconds)
  "Format non-negative duration SECONDS compactly."
  (let* ((seconds (max 0 (floor seconds)))
         (days (floor seconds 86400))
         (hours (floor (mod seconds 86400) 3600))
         (minutes (floor (mod seconds 3600) 60))
         (remaining (mod seconds 60)))
    (cond
     ((> days 0) (format "%dd %02dh" days hours))
     ((> hours 0) (format "%dh %02dm" hours minutes))
     ((> minutes 0) (format "%dm %02ds" minutes remaining))
     (t (format "%ds" remaining)))))

(defun quota-dashboard--countdown (value)
  "Return the live countdown until ISO-8601 VALUE."
  (if-let* ((target (quota-dashboard--parse-time value)))
      (let ((remaining (- target (float-time))))
        (if (<= remaining 0)
            "now"
          (quota-dashboard--duration remaining)))
    "unavailable"))

(defun quota-dashboard--age (value)
  "Return a compact age label for ISO-8601 VALUE."
  (if-let* ((timestamp (quota-dashboard--parse-time value)))
      (let ((age (max 0 (floor (- (float-time) timestamp)))))
        (cond
         ((< age 5) "just now")
         ((< age 60) (format "%ds ago" age))
         ((< age 3600) (format "%dm ago" (floor age 60)))
         ((< age 86400) (format "%dh %02dm ago"
                               (floor age 3600)
                               (floor (mod age 3600) 60)))
         (t (format "%dd ago" (floor age 86400)))))
    "unknown"))

(defun quota-dashboard--compact-number (value)
  "Format numeric VALUE for a token count."
  (let ((number (quota-dashboard--number value)))
    (cond
     ((null number) "—")
     ((>= number 1000000) (format "%.1fM" (/ number 1000000.0)))
     ((>= number 1000) (format "%.1fK" (/ number 1000.0)))
     (t (format "%.0f" number)))))

(defun quota-dashboard--percent-label (value)
  "Format percentage VALUE for a fixed-width column."
  (if-let* ((number (quota-dashboard--number value)))
      (format "%3.0f%%" number)
    "  —"))

(defun quota-dashboard--bar-face (percent)
  "Return the face for a bar with PERCENT usage."
  (if (and (numberp percent) (>= percent 85))
      'quota-dashboard-bar-warning
    'quota-dashboard-bar-filled))

(defun quota-dashboard--bar (percent)
  "Return a propertized quota bar for PERCENT usage."
  (let ((width (max 1 quota-dashboard-bar-width))
        (number (quota-dashboard--number percent)))
    (if number
        (let* ((bounded (max 0 (min 100 number)))
               (filled (round (* width (/ bounded 100.0)))))
          (concat
           "["
           (propertize (make-string filled ?█)
                       'face (quota-dashboard--bar-face bounded))
           (propertize (make-string (- width filled) ?░)
                       'face 'quota-dashboard-bar-empty)
           "]"))
      (concat
       "["
       (propertize (make-string width ?·) 'face 'quota-dashboard-bar-empty)
       "]"))))

(defun quota-dashboard--pace (value)
  "Return a cons of pace text and face for percentage-point VALUE."
  (let ((number (quota-dashboard--number value)))
    (cond
     ((null number) (cons "pace —" 'quota-dashboard-meta))
     ((> number 0.5)
      (cons (format "ahead %+.1fpp" number) 'quota-dashboard-ahead))
     ((< number -0.5)
      (cons (format "behind %.1fpp" (abs number)) 'quota-dashboard-behind))
     (t (cons "on pace" 'quota-dashboard-meta)))))

(defun quota-dashboard--burn (window)
  "Return a burn-rate label for WINDOW."
  (let ((tokens (quota-dashboard--number
                 (quota-dashboard--get 'burnRateTokensPerHour window)))
        (percent (quota-dashboard--number
                  (quota-dashboard--get 'burnRatePercentPerHour window))))
    (cond
     (tokens (format "burn %.1fK/h" (/ tokens 1000.0)))
     (percent (format "burn %.1fpp/h" percent))
     (t "burn —"))))

(defun quota-dashboard--token-usage (window)
  "Return a token usage label for WINDOW when token counts are present."
  (let ((used (quota-dashboard--get 'usedTokens window))
        (limit (quota-dashboard--get 'limitTokens window)))
    (cond
     ((and (numberp used) (numberp limit))
      (format "%s/%s tok"
              (quota-dashboard--compact-number used)
              (quota-dashboard--compact-number limit)))
     ((numberp used)
      (format "%s tok" (quota-dashboard--compact-number used)))
     (t nil))))

(defun quota-dashboard--reset-label (window)
  "Return the live reset countdown for WINDOW."
  (quota-dashboard--countdown (quota-dashboard--get 'resetAt window)))

(defun quota-dashboard--snapshot-path ()
  "Return the first readable Quota snapshot path."
  (catch 'path
    (dolist (path (delete-dups
                   (list quota-dashboard-snapshot-file
                         quota-dashboard-fallback-snapshot-file)))
      (when (file-readable-p path)
        (throw 'path path)))))

(defun quota-dashboard--read-snapshot ()
  "Read and return the current Quota snapshot as (PATH . ALIST)."
  (let ((path (quota-dashboard--snapshot-path)))
    (unless path
      (user-error "Quota snapshot not found"))
    (cons path
          (with-temp-buffer
            (insert-file-contents path)
            (let ((json-object-type 'alist)
                  (json-array-type 'list)
                  (json-key-type 'symbol))
              (json-read))))))

(defun quota-dashboard--insert-window (window)
  "Insert one quota WINDOW row."
  (let* ((label (or (quota-dashboard--get 'label window) "Window"))
         (used (quota-dashboard--get 'usedPercent window))
         (headroom (quota-dashboard--get 'headroomPercent window))
         (pace (quota-dashboard--pace
                (quota-dashboard--get 'paceDeltaPercentagePoints window)))
         (token-usage (quota-dashboard--token-usage window)))
    (insert
     (format "  %-8s %s %5s  headroom %5s  reset %-10s  "
             label
             (quota-dashboard--bar used)
             (quota-dashboard--percent-label used)
             (quota-dashboard--percent-label headroom)
             (quota-dashboard--reset-label window)))
    (insert (propertize (car pace) 'face (cdr pace)))
    (insert (format "  %s\n" (quota-dashboard--burn window)))
    (when token-usage
      (insert (propertize (format "    usage %s\n" token-usage)
                          'face 'quota-dashboard-meta)))
    (when (not used)
      (let ((note (quota-dashboard--get 'sourceNote window)))
        (when (and note (not (string-empty-p note)))
          (insert (propertize (format "    %s\n" note)
                              'face 'quota-dashboard-meta)))))))

(defun quota-dashboard--insert-provider (provider)
  "Insert one quota PROVIDER section."
  (let ((provider-id (quota-dashboard--get 'id provider)))
    (insert (propertize (or (quota-dashboard--get 'name provider) "Provider")
                      'face 'quota-dashboard-provider))
    (let ((status (quota-dashboard--get 'status provider)))
      (when (and status (not (string= status "ready")))
        (insert (propertize (format "  [%s]" status)
                            'face 'quota-dashboard-meta))))
    (insert "\n")
    (dolist (window (quota-dashboard--get 'windows provider))
      (unless (and (equal provider-id "codex")
                   (equal (quota-dashboard--get 'id window) "5h"))
        (quota-dashboard--insert-window window)))
    (let ((detail (quota-dashboard--get 'detail provider)))
      (when (and detail (not (string-empty-p detail)))
        (insert (propertize (format "  %s\n" detail)
                            'face 'quota-dashboard-meta))))
    (insert "\n")))

(defun quota-dashboard--deepseek-state (value)
  "Return a display label for DeepSeek STATE VALUE."
  (cond
   ((equal value "peak") "Peak now")
   ((equal value "offPeak") "Off-peak")
   ((equal value "notConfigured") "Not configured")
   ((and value (not (string-empty-p value))) value)
   (t "Unknown")))

(defun quota-dashboard--insert-deepseek (deepseek)
  "Insert the DeepSeek schedule section from DEEPSEEK alist."
  (insert (propertize "DeepSeek" 'face 'quota-dashboard-provider) "\n")
  (let ((state (quota-dashboard--get 'state deepseek))
        (description (or (quota-dashboard--get 'windowDescription deepseek)
                         "Peak window unavailable"))
        (next-change (quota-dashboard--get 'nextChangeAt deepseek)))
    (insert (format "  %-14s  next change %-10s\n"
                    (quota-dashboard--deepseek-state state)
                    (quota-dashboard--countdown next-change)))
    (insert (propertize (format "  %s\n" description)
                        'face 'quota-dashboard-meta))
    (when-let* ((detail (quota-dashboard--get 'detail deepseek)))
      (insert (propertize (format "  %s\n" detail)
                          'face 'quota-dashboard-meta)))))

(defun quota-dashboard--render (snapshot path)
  "Render SNAPSHOT read from PATH in the current buffer."
  (let ((position (point))
        (inhibit-read-only t)
        (generated (quota-dashboard--get 'generatedAt snapshot)))
    (erase-buffer)
    (insert (propertize "Quota" 'face 'quota-dashboard-title) "\n")
    (insert (propertize
             (format "Updated %s  ·  local snapshot\n\n"
                     (quota-dashboard--age generated))
             'face 'quota-dashboard-meta))
    (dolist (provider (quota-dashboard--get 'providers snapshot))
      (quota-dashboard--insert-provider provider))
    (quota-dashboard--insert-deepseek
     (or (quota-dashboard--get 'deepSeek snapshot) '()))
    (insert "\n"
            (propertize "g/r refresh  ·  q quit  ·  " 'face 'quota-dashboard-meta)
            (propertize (file-name-nondirectory path) 'face 'quota-dashboard-meta)
            "\n")
    (goto-char (min position (point-max)))
    (set-buffer-modified-p nil)))

(defun quota-dashboard--render-error (message)
  "Render MESSAGE in the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Quota" 'face 'quota-dashboard-title) "\n\n")
    (insert (propertize message 'face 'error) "\n")
    (insert (propertize "g/r refresh  ·  q quit\n" 'face 'quota-dashboard-meta))
    (set-buffer-modified-p nil)))

(defun quota-dashboard--refresh-buffer ()
  "Refresh the current Quota dashboard buffer from disk."
  (condition-case err
      (let ((result (quota-dashboard--read-snapshot)))
        (quota-dashboard--render (cdr result) (car result)))
    (error (quota-dashboard--render-error (error-message-string err)))))

(defun quota-dashboard--auto-refresh (buffer)
  "Refresh live dashboard BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'quota-dashboard-mode)
        (quota-dashboard--refresh-buffer)))))

(defun quota-dashboard--stop-timer ()
  "Stop the current buffer's Quota refresh timer."
  (when (timerp quota-dashboard--refresh-timer)
    (cancel-timer quota-dashboard--refresh-timer))
  (setq quota-dashboard--refresh-timer nil))

(defun quota-dashboard--start-timer ()
  "Start the current buffer's Quota refresh timer."
  (quota-dashboard--stop-timer)
  (setq quota-dashboard--refresh-timer
        (run-at-time quota-dashboard-refresh-interval
                     quota-dashboard-refresh-interval
                     #'quota-dashboard--auto-refresh
                     (current-buffer))))

(define-derived-mode quota-dashboard-mode special-mode "Quota"
  "Major mode for the local Quota dashboard."
  (setq-local truncate-lines t)
  (setq-local cursor-type nil)
  (add-hook 'kill-buffer-hook #'quota-dashboard--stop-timer nil t)
  (quota-dashboard--start-timer))

;;;###autoload
(defun quota-dashboard-refresh ()
  "Refresh the Quota dashboard buffer."
  (interactive)
  (let ((buffer (if (derived-mode-p 'quota-dashboard-mode)
                    (current-buffer)
                  (get-buffer-create quota-dashboard-buffer-name))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'quota-dashboard-mode)
        (quota-dashboard-mode))
      (quota-dashboard--refresh-buffer))
    (unless (eq buffer (current-buffer))
      (pop-to-buffer buffer))))

;;;###autoload
(defun quota-dashboard ()
  "Open the local Quota dashboard."
  (interactive)
  (let ((buffer (get-buffer-create quota-dashboard-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'quota-dashboard-mode)
        (quota-dashboard-mode))
      (quota-dashboard--refresh-buffer))
    (pop-to-buffer buffer)))

(provide 'quota-dashboard)
;;; quota-dashboard.el ends here
