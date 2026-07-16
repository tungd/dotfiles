;;; tterm-dashboard.el --- Dashboard for tmux-backed tterm -*- lexical-binding: t; -*-

;;; Commentary:
;; Host-grouped dashboard for tmux-backed tterm windows.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'tterm)

(defvar tterm-dashboard-buffer-name "*tterm-dashboard*"
  "Buffer name used for the tterm dashboard.")

(defcustom tterm-dashboard-refresh-interval 10.0
  "Seconds between automatic dashboard refreshes while the dashboard is visible."
  :type 'number
  :group 'tterm)

(defvar-local tterm-dashboard--refresh-timer nil
  "Buffer-local automatic refresh timer for the tterm dashboard.")

(defvar-local tterm-dashboard--last-snapshot nil
  "Last rendered snapshot.
Used to detect changes and avoid unnecessary point moves.")

(defvar-local tterm-dashboard--refresh-in-progress nil
  "Non-nil when an async dashboard refresh is pending for this buffer.")

(defvar-local tterm-dashboard--pending-handle nil
  "Dashboard window handle to restore after an async refresh.")

(defvar-local tterm-dashboard--refresh-job-handle nil
  "Async job handle from `tterm-bridge-command-async', for cancellation.")

(defvar tterm-dashboard--focus-callback-installed nil
  "Non-nil when dashboard focus callback is installed.")

(defvar tterm-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    map)
  "Keymap for `tterm-dashboard-mode'.")

(define-key tterm-dashboard-mode-map (kbd "RET") #'tterm-dashboard-select)
(define-key tterm-dashboard-mode-map (kbd "n") #'tterm-dashboard-next)
(define-key tterm-dashboard-mode-map (kbd "p") #'tterm-dashboard-previous)
(define-key tterm-dashboard-mode-map (kbd "g") #'tterm-dashboard-refresh)
(define-key tterm-dashboard-mode-map (kbd "d") #'tterm-dashboard-detach-window)
(define-key tterm-dashboard-mode-map (kbd "k") #'tterm-dashboard-kill-window)
(define-key tterm-dashboard-mode-map (kbd "c") #'tterm-dashboard-create-window)

(defun tterm-dashboard--decode-command-text (text)
  "Decode dashboard command TEXT returned as UTF-8 bytes."
  (if (and (not (multibyte-string-p text))
           (string-match-p "[\200-\377]" text))
      (decode-coding-string text 'utf-8 t)
    text))

(defun tterm-dashboard--unescape-field (value)
  "Decode one escaped dashboard field VALUE."
  (let ((index 0)
        (length (length value))
        (out nil))
    (while (< index length)
      (let ((char (aref value index)))
        (if (and (= char ?\\) (< (1+ index) length))
            (let ((next (aref value (1+ index))))
              (push (pcase next
                      (?n ?\n)
                      (?t ?\t)
                      (?\\ ?\\)
                      (_ next))
                    out)
              (setq index (+ index 2)))
          (push char out)
          (setq index (1+ index)))))
    (apply #'string (nreverse out))))

(defun tterm-dashboard--escape-field (value)
  "Escape one dashboard field VALUE for tab-separated payloads."
  (let ((index 0)
        (length (length value))
        (out nil))
    (while (< index length)
      (pcase (aref value index)
        (?\\ (push "\\\\" out))
        (?\t (push "\\t" out))
        (?\n (push "\\n" out))
        (char (push (string char) out)))
      (setq index (1+ index)))
    (apply #'concat (nreverse out))))

(defun tterm-dashboard--number-or-nil (value)
  "Return numeric VALUE, or nil when VALUE is blank."
  (unless (string-empty-p value)
    (string-to-number value)))

(defun tterm-dashboard--make-handle
    (host namespace socket session window-id pane-id &optional identity)
  "Return a stable tmux handle plist."
  (let ((handle (list :host host
                      :namespace namespace
                      :socket socket
                      :session session
                      :window-id window-id
                      :pane-id pane-id)))
    (if (and identity (not (string-empty-p identity)))
        (plist-put handle :identity identity)
      handle)))

(defun tterm-dashboard--group-key (host socket session)
  "Return dashboard group key for HOST, SOCKET, and SESSION."
  (list :group host socket session))

(defun tterm-dashboard--decode (text)
  "Decode dashboard TEXT into host plists."
  (setq text (tterm-dashboard--decode-command-text text))
  (let ((hosts (make-hash-table :test 'equal)))
    (dolist (line (split-string text "\n" t))
      (let ((fields (mapcar #'tterm-dashboard--unescape-field
                            (split-string line "\t"))))
        (pcase fields
          (`("H" ,host ,session ,socket)
           (let ((key (tterm-dashboard--group-key host socket session)))
             (puthash key (list :host host
                                :session session
                                :socket socket
                                :windows nil)
                      hosts)))
          (`("W" ,terminal-id ,host ,namespace ,socket ,session ,window-id
             ,pane-id ,name ,process ,cwd ,attached ,status ,notification
             . ,rest)
           (let* ((key (tterm-dashboard--group-key host socket session))
                  (entry (or (gethash key hosts)
                             (list :host host
                                   :session session
                                   :socket socket
                                   :windows nil)))
                  (id (tterm-dashboard--number-or-nil terminal-id))
                  (notification (unless (string-empty-p notification)
                                  notification))
                  (identity (car rest))
                  (unread (or (tterm-dashboard--number-or-nil
                               (or (cadr rest) ""))
                              0))
                  (handle (tterm-dashboard--make-handle
                           host namespace socket session window-id pane-id
                           identity))
                  (window (list :terminal-id id
                                :window-id window-id
                                :pane-id pane-id
                                :identity identity
                                :handle handle
                                :name name
                                :process process
                                :cwd cwd
                                :attached attached
                                :status status
                                :notification notification
                                :unread-notifications unread)))
             (plist-put entry :session session)
             (plist-put entry :socket socket)
             (plist-put entry :windows
                        (append (plist-get entry :windows) (list window)))
             (puthash key entry hosts))))))
    (let (result)
      (maphash (lambda (_ value)
                 (push value result))
               hosts)
      (sort result (lambda (left right)
                     (string<
                      (format "%s\t%s\t%s"
                              (plist-get left :host)
                              (plist-get left :session)
                              (plist-get left :socket))
                      (format "%s\t%s\t%s"
                              (plist-get right :host)
                              (plist-get right :session)
                              (plist-get right :socket))))))))

(defun tterm-dashboard--visible-p (buffer)
  "Return non-nil when dashboard BUFFER is displayed in any frame."
  (and (buffer-live-p buffer)
       (get-buffer-window-list buffer nil t)))

(defun tterm-dashboard--auto-refresh (buffer)
  "Refresh dashboard BUFFER if it is still live and visible."
  (when (and (buffer-live-p buffer)
             (tterm-dashboard--visible-p buffer))
    (with-current-buffer buffer
      (when (derived-mode-p 'tterm-dashboard-mode)
        (tterm-dashboard--refresh-async t)))))

(defun tterm-dashboard--frame-visible-p ()
  "Return non-nil when any frame is visible (not minimized/iconified)."
  (cl-some (lambda (frame)
             (and (frame-live-p frame)
                  (frame-visible-p frame)))
           (frame-list)))

(defun tterm-dashboard--manage-auto-refresh ()
  "Start or stop auto-refresh timers based on buffer/frame visibility.
Intended for focus, window configuration, and kill-emacs callbacks."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'tterm-dashboard-mode)
        (if (and (tterm-dashboard--visible-p buf)
                 (tterm-dashboard--frame-visible-p))
            (tterm-dashboard--start-auto-refresh)
          (tterm-dashboard--cancel-auto-refresh))))))

(defun tterm-dashboard--setup-visibility-hooks ()
  "Set up hooks to manage auto-refresh based on visibility."
  (unless tterm-dashboard--focus-callback-installed
    (add-function :after after-focus-change-function
                  #'tterm-dashboard--manage-auto-refresh)
    (setq tterm-dashboard--focus-callback-installed t))
  (add-hook 'window-configuration-change-hook #'tterm-dashboard--manage-auto-refresh))

(defun tterm-dashboard--teardown-visibility-hooks ()
  "Remove visibility hooks when no dashboard buffers remain."
  (unless (cl-some (lambda (buf)
                     (with-current-buffer buf
                       (derived-mode-p 'tterm-dashboard-mode)))
                   (buffer-list))
    (when tterm-dashboard--focus-callback-installed
      (remove-function after-focus-change-function
                       #'tterm-dashboard--manage-auto-refresh)
      (setq tterm-dashboard--focus-callback-installed nil))
    (remove-hook 'window-configuration-change-hook #'tterm-dashboard--manage-auto-refresh)))

(defun tterm-dashboard--start-auto-refresh ()
  "Start automatic refresh for the current dashboard buffer."
  (unless tterm-dashboard--refresh-timer
    (setq-local
     tterm-dashboard--refresh-timer
     (run-at-time tterm-dashboard-refresh-interval
                  tterm-dashboard-refresh-interval
                  #'tterm-dashboard--auto-refresh
                  (current-buffer)))
    (tterm-dashboard--setup-visibility-hooks)))

(defun tterm-dashboard--cancel-auto-refresh ()
  "Cancel the buffer-local dashboard refresh timer."
  (when tterm-dashboard--refresh-timer
    (when (timerp tterm-dashboard--refresh-timer)
      (cancel-timer tterm-dashboard--refresh-timer))
    (setq-local tterm-dashboard--refresh-timer nil)
    (tterm-dashboard--teardown-visibility-hooks)))

(define-derived-mode tterm-dashboard-mode special-mode "tterm-dashboard"
  "Dashboard for tmux-backed tterm windows."
  (add-hook 'change-major-mode-hook
            #'tterm-dashboard--cancel-auto-refresh nil t)
  (add-hook 'kill-buffer-hook #'tterm-dashboard--cancel-auto-refresh nil t)
  (tterm-dashboard--start-auto-refresh))

(defun tterm-dashboard--window-position-at-or-after (position)
  "Return the next dashboard window row position at or after POSITION."
  (let ((pos position))
    (while (and pos (< pos (point-max))
                (not (get-text-property pos 'tterm-handle)))
      (setq pos (next-single-property-change pos 'tterm-handle nil
                                             (point-max))))
    (and pos (< pos (point-max)) pos)))

(defun tterm-dashboard--line-property-at-point (property)
  "Return dashboard row PROPERTY at point, searching the current line."
  (or (get-text-property (point) property)
      (save-excursion
        (beginning-of-line)
        (let ((end (line-end-position))
              value)
          (while (and (< (point) end) (not value))
            (setq value (get-text-property (point) property))
            (goto-char (or (next-single-property-change
                            (point) property nil end)
                           end)))
          value))))

(defun tterm-dashboard--window-handle-at-point ()
  "Return stable dashboard window handle at point, or nil."
  (tterm-dashboard--line-property-at-point 'tterm-handle))

(defun tterm-dashboard--terminal-id-at-point ()
  "Return live terminal id at point, or nil."
  (tterm-dashboard--line-property-at-point 'tterm-terminal-id))

(defun tterm-dashboard--host-at-point ()
  "Return dashboard host at point, or nil."
  (or (tterm-dashboard--line-property-at-point 'tterm-host)
      (plist-get (tterm-dashboard--window-handle-at-point) :host)))

(defun tterm-dashboard--goto-window-handle (handle)
  "Move point to dashboard row for HANDLE.
Return non-nil when such a row exists."
  (let ((pos (point-min))
        found)
    (while (and (< pos (point-max)) (not found))
      (when (equal (get-text-property pos 'tterm-handle) handle)
        (setq found pos))
      (setq pos (or (next-single-property-change pos 'tterm-handle nil
                                                 (point-max))
                    (point-max))))
    (when found
      (goto-char found)
      t)))

(defun tterm-dashboard--buffer-for-terminal-id (id)
  "Return live tterm buffer for terminal ID."
  (and id
       (cl-find-if
        (lambda (buffer)
          (with-current-buffer buffer
            (and tterm--terminal
                 (= (tterm-id tterm--terminal) id))))
        (tterm--buffers))))

(defun tterm-dashboard--encode-reattach-payload (handle rows cols)
  "Encode tmux HANDLE and terminal ROWS/COLS for reattach-window."
  (let ((lines
         (list (format "host\t%s"
                       (tterm-dashboard--escape-field (plist-get handle :host)))
               (format "namespace\t%s"
                       (tterm-dashboard--escape-field
                        (plist-get handle :namespace)))
               (format "socket\t%s"
                       (tterm-dashboard--escape-field (plist-get handle :socket)))
               (format "session\t%s"
                       (tterm-dashboard--escape-field (plist-get handle :session)))
               (format "window\t%s"
                       (tterm-dashboard--escape-field
                        (plist-get handle :window-id)))
               (format "pane\t%s"
                       (tterm-dashboard--escape-field (plist-get handle :pane-id)))
               (format "rows\t%d" rows)
               (format "cols\t%d" cols))))
    (when-let* ((identity (plist-get handle :identity)))
      (setq lines
            (append lines
                    (list (format "identity\t%s"
                                  (tterm-dashboard--escape-field identity))))))
    (mapconcat #'identity lines "\n")))

(defun tterm-dashboard--encode-mark-read-payload (handle &optional terminal-id)
  "Encode HANDLE and optional TERMINAL-ID for mark-read."
  (let ((lines nil))
    (when terminal-id
      (push (format "terminal-id\t%d" terminal-id) lines))
    (when (listp handle)
      (dolist (entry '(("host" . :host)
                       ("namespace" . :namespace)
                       ("socket" . :socket)
                       ("session" . :session)
                       ("window" . :window-id)
                       ("pane" . :pane-id)
                       ("identity" . :identity)))
        (when-let* ((value (plist-get handle (cdr entry))))
          (push (format "%s\t%s"
                        (car entry)
                        (tterm-dashboard--escape-field value))
                lines))))
    (mapconcat #'identity (nreverse lines) "\n")))

(defun tterm-dashboard--mark-read (handle &optional terminal-id)
  "Clear backend unread state for HANDLE or TERMINAL-ID."
  (ignore-errors
    (tterm-bridge-command
     (or terminal-id 0)
     "mark-read"
     (tterm-dashboard--encode-mark-read-payload handle terminal-id))))

(defun tterm-dashboard--decode-reattach-response (text)
  "Decode reattach-window response TEXT."
  (let* ((text (tterm-dashboard--decode-command-text text))
         (fields (mapcar #'tterm-dashboard--unescape-field
                         (split-string text "\t"))))
    (pcase fields
      (`("A" ,id ,host ,cwd ,window-id ,pane-id ,name . ,rest)
       (list :id (string-to-number id)
             :host host
             :cwd cwd
             :window-id window-id
             :pane-id pane-id
             :name name
             :identity (car rest)))
      (`("E" ,message)
       (user-error "%s" message))
      (_
       (user-error "Invalid reattach-window response: %s" text)))))

(defun tterm-dashboard--reattach-window (handle &optional callback)
  "Reattach tmux HANDLE and open the resulting tterm buffer.
When CALLBACK is non-nil, call it with the terminal buffer after
reattaching, instead of switching to it synchronously."
  (unless (and (listp handle) (plist-member handle :window-id))
    (user-error "No stable tmux handle for this row"))
  (let* ((grid (tterm--window-grid-size))
         (rows (car grid))
         (cols (cdr grid))
         (payload (tterm-dashboard--encode-reattach-payload handle rows cols)))
    (message "Reattaching tmux window…")
    (tterm-bridge-command-async
     0 "reattach-window" payload
     (lambda (result error-p)
       (if error-p
           (message "Reattach failed: %s" error-p)
         (let ((response (tterm-dashboard--decode-reattach-response result)))
           (tterm--attach-terminal-buffer
            (plist-get response :id)
            rows
            cols
            (plist-get response :host)
            (plist-get response :cwd)
            (plist-get response :window-id)
            (plist-get response :pane-id)
            (plist-get response :name)
            t)
           (let ((buffer (tterm-dashboard--buffer-for-terminal-id
                          (plist-get response :id))))
             (if callback
                 (funcall callback buffer)
               (when buffer
                 (switch-to-buffer buffer))))))))))

(defun tterm-dashboard-select-window (handle &optional terminal-id)
  "Select dashboard window by stable HANDLE and optional TERMINAL-ID.
When the window is detached, reattach asynchronously."
  (let ((buffer (tterm-dashboard--buffer-for-terminal-id terminal-id)))
    (if buffer
        (progn
          (tterm-dashboard--mark-read handle terminal-id)
          (switch-to-buffer buffer))
      (tterm-dashboard--reattach-window handle))))

(defun tterm-dashboard-select ()
  "Select the tterm buffer on the current dashboard row."
  (interactive)
  (let ((handle (tterm-dashboard--window-handle-at-point))
        (id (tterm-dashboard--terminal-id-at-point)))
    (unless handle
      (user-error "No tterm window on this line"))
    (tterm-dashboard-select-window handle id)))

(defun tterm-dashboard-detach-window ()
  "Detach the attached tterm window on the current dashboard row.
Disposes the attached tterm buffer to stop its redraw timers."
  (interactive)
  (let ((id (tterm-dashboard--terminal-id-at-point)))
    (unless id
      (user-error "No attached tterm window on this line"))
    (tterm-bridge-command id "detach" "")
    (when-let* ((buffer (tterm--buffer-for-terminal-id id)))
      (with-current-buffer buffer
        (tterm--dispose-terminal-buffer)))
    (tterm-dashboard-refresh)))

(defun tterm-dashboard-kill-window ()
  "Kill the attached tterm window on the current dashboard row.
Disposes and kills the attached tterm buffer."
  (interactive)
  (let ((id (tterm-dashboard--terminal-id-at-point)))
    (unless id
      (user-error "No attached tterm window on this line"))
    (ignore-errors
      (tterm-bridge-command id "kill-window" ""))
    (when-let* ((buffer (tterm--buffer-for-terminal-id id)))
      (with-current-buffer buffer
        (tterm--dispose-terminal-buffer))
      (kill-buffer buffer))
    (tterm-dashboard-refresh)))

(defun tterm-dashboard-create-window ()
  "Create a new tterm window on the host at point."
  (interactive)
  (let* ((host (or (tterm-dashboard--host-at-point) "local"))
         (grid (tterm--window-grid-size))
         (rows (car grid))
         (cols (cdr grid))
         (cwd (tterm--cwd-for-host host))
         (id (tterm--connect rows cols host
                             (tterm--normalize-start-cwd cwd))))
    (tterm--attach-terminal-buffer id rows cols host cwd)))

(defun tterm-dashboard-next (&optional count)
  "Move to the next tterm window row.
With COUNT, move that many rows."
  (interactive "p")
  (dotimes (_ (or count 1))
    (let ((current-handle (tterm-dashboard--window-handle-at-point))
          (pos (point))
          next)
      (while (and (< pos (point-max)) (not next))
        (setq pos (or (next-single-property-change
                       pos 'tterm-handle nil (point-max))
                      (point-max)))
        (let ((handle (get-text-property pos 'tterm-handle)))
          (when (and handle (not (equal handle current-handle)))
            (setq next pos))))
      (if next
          (tterm-dashboard--goto-window-handle
           (get-text-property next 'tterm-handle))
        (user-error "No next tterm window")))))

(defun tterm-dashboard-previous (&optional count)
  "Move to the previous tterm window row.
With COUNT, move that many rows."
  (interactive "p")
  (dotimes (_ (or count 1))
    (let ((current-handle (tterm-dashboard--window-handle-at-point))
          (pos (point-min))
          previous-handle)
      (while (< pos (point))
        (let ((handle (get-text-property pos 'tterm-handle)))
          (when (and handle
                     (not (equal handle current-handle))
                     (not (equal handle previous-handle)))
            (setq previous-handle handle)))
        (setq pos (or (next-single-property-change
                       pos 'tterm-handle nil (point))
                      (point))))
      (if previous-handle
          (tterm-dashboard--goto-window-handle previous-handle)
        (user-error "No previous tterm window")))))

(defun tterm-dashboard--window-action (button)
  "Handle dashboard window BUTTON activation."
  (tterm-dashboard-select-window (button-get button 'tterm-handle)
                                 (button-get button 'tterm-terminal-id)))

(defun tterm-dashboard--insert-window (window)
  "Insert one dashboard WINDOW row."
  (let ((name (plist-get window :name))
        (process (plist-get window :process))
        (window-id (plist-get window :window-id))
        (cwd (plist-get window :cwd))
        (attached (plist-get window :attached))
        (status (plist-get window :status))
        (notification (plist-get window :notification))
        (id (plist-get window :terminal-id))
        (handle (plist-get window :handle))
        (row-props nil))
    (setq row-props (list 'tterm-terminal-id id
                          'tterm-handle handle
                          'tterm-host (plist-get handle :host)
                          'tterm-window-id window-id
                          'tterm-pane-id (plist-get window :pane-id)
                          'mouse-face 'highlight))
    (let ((row-start (point)))
      (insert "  ")
      (insert-text-button (or name window-id)
                          'action #'tterm-dashboard--window-action
                          'follow-link t
                          'tterm-terminal-id id
                          'tterm-handle handle
                          'tterm-window-id window-id
                          'tterm-pane-id (plist-get window :pane-id)
                          'mouse-face 'highlight)
      (when (and process
                 (not (string-empty-p process))
                 (not (equal process name)))
        (insert (propertize (format "  %s" process) 'face 'shadow)))
      (insert (format "  %s  %s" attached window-id))
      (when (and status (not (string-empty-p status)))
        (insert (propertize (format "  [%s]" status) 'face 'bold)))
      (when-let* ((unread (plist-get window :unread-notifications)))
        (when (> unread 0)
          (insert (propertize (format "  [notify:%d]" unread)
                              'face 'tterm-notification-mode-line))))
      (add-text-properties row-start (point) row-props)
      (insert "\n"))
    (let ((line-start (point)))
      (insert (propertize (format "    %s\n" cwd) 'face 'shadow))
      (add-text-properties line-start (point) row-props))
    (when (and notification (not (string-empty-p notification)))
      (let ((line-start (point)))
        (insert (propertize (format "    %s\n" notification) 'face 'italic))
        (add-text-properties line-start (point) row-props)))))

(defun tterm-dashboard--render (snapshot)
  "Render dashboard SNAPSHOT in the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (if (not snapshot)
        (insert (propertize "No tterm sessions\n" 'face 'shadow))
      (dolist (host snapshot)
        (let ((host-start (point))
              (host-name (plist-get host :host)))
          (insert (propertize host-name 'face 'bold) "\n")
          (add-text-properties host-start (point) (list 'tterm-host host-name))
          (let ((subtitle-start (point)))
            (insert
             (propertize
              (format "  %s  %s\n"
                      (plist-get host :session)
                      (plist-get host :socket))
              'face 'shadow))
            (add-text-properties subtitle-start (point)
                                 (list 'tterm-host host-name)))
          (if-let* ((windows (plist-get host :windows)))
              (dolist (window windows)
                (tterm-dashboard--insert-window window))
            (let ((empty-start (point)))
              (insert (propertize "  No windows discovered\n" 'face 'shadow))
              (add-text-properties empty-start (point)
                                   (list 'tterm-host host-name)))))
        (insert "\n"))))
  (goto-char (or (tterm-dashboard--window-position-at-or-after (point-min))
                 (point-min))))

(defun tterm-dashboard--refresh-async (&optional local-only)
  "Start an async dashboard refresh.
When called interactively, prefix arg forces a new refresh even if one
is already in progress.
When LOCAL-ONLY is non-nil, discover local tmux windows but skip remote SSH."
  (interactive)
  (when (and tterm-dashboard--refresh-in-progress current-prefix-arg)
    (when tterm-dashboard--refresh-job-handle
      (tterm-bridge-command-cancel tterm-dashboard--refresh-job-handle)
      (setq tterm-dashboard--refresh-job-handle nil))
    (setq tterm-dashboard--refresh-in-progress nil))
  (when tterm-dashboard--refresh-in-progress
    (user-error "Dashboard refresh already in progress; C-u g to retry"))
  (setq tterm-dashboard--refresh-in-progress t)
  (setq tterm-dashboard--pending-handle
        (tterm-dashboard--window-handle-at-point))
  (let ((buffer (current-buffer)))
    (let ((job-handle
           (tterm-bridge-command-async
            0 (if local-only "dashboard-local" "dashboard") ""
            (lambda (result error-p)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (setq tterm-dashboard--refresh-job-handle nil)
                  (if error-p
                      (progn
                        (setq tterm-dashboard--refresh-in-progress nil)
                        (message "Dashboard refresh failed: %s" error-p))
                    (let ((snapshot (tterm-dashboard--decode result)))
                      (if (equal snapshot tterm-dashboard--last-snapshot)
                          ;; No change: just clear the in-progress flag
                          (setq tterm-dashboard--refresh-in-progress nil)
                        ;; Data changed: render and restore position
                        (setq tterm-dashboard--last-snapshot snapshot)
                        (tterm-dashboard--render snapshot)
                        (when tterm-dashboard--pending-handle
                          (tterm-dashboard--goto-window-handle
                           tterm-dashboard--pending-handle))
                        (setq tterm-dashboard--pending-handle nil)
                        (setq tterm-dashboard--refresh-in-progress nil))))))))))
      (setq tterm-dashboard--refresh-job-handle job-handle))))

;;;###autoload
(defun tterm-dashboard-refresh (&optional full)
  "Refresh the tterm dashboard buffer.
When called from a non-dashboard buffer, target
`tterm-dashboard-buffer-name' instead of the current buffer."
  (interactive (list t))
  (if (derived-mode-p 'tterm-dashboard-mode)
      (tterm-dashboard--refresh-async (not full))
    (let ((buffer (get-buffer-create tterm-dashboard-buffer-name)))
      (with-current-buffer buffer
        (unless (derived-mode-p 'tterm-dashboard-mode)
          (tterm-dashboard-mode))
        (tterm-dashboard--refresh-async (not full))))))

;;;###autoload
(defun tterm-dashboard ()
  "Open the tterm dashboard."
  (interactive)
  (let ((buffer (get-buffer-create tterm-dashboard-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'tterm-dashboard-mode)
        (tterm-dashboard-mode))
      (tterm-dashboard-refresh))
    (switch-to-buffer buffer)))

(provide 'tterm-dashboard)

;;; tterm-dashboard.el ends here
