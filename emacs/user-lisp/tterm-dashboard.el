;;; tterm-dashboard.el --- Dashboard for native tterm -*- lexical-binding: t; -*-

;;; Commentary:
;; Host-grouped dashboard for Lisp-owned terminal buffers.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'tterm)
(require 'tterm-attention)

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
        (tterm-dashboard--refresh)))))

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
                       (and (derived-mode-p 'tterm-dashboard-mode)
                            tterm-dashboard--refresh-timer)))
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
  "Dashboard for native tterm windows."
  (add-hook 'change-major-mode-hook
            #'tterm-dashboard--cancel-auto-refresh nil t)
  (add-hook 'kill-buffer-hook #'tterm-dashboard--cancel-auto-refresh nil t)
  (tterm-dashboard--start-auto-refresh))

(defun tterm-dashboard--window-position-at-or-after (position)
  "Return the next dashboard window row position at or after POSITION."
  (let ((pos position))
    (while (and pos (< pos (point-max))
                (not (get-text-property pos 'tterm-terminal-id)))
      (setq pos (next-single-property-change pos 'tterm-terminal-id nil
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

(defun tterm-dashboard--terminal-id-at-point ()
  "Return live terminal id at point, or nil."
  (tterm-dashboard--line-property-at-point 'tterm-terminal-id))

(defun tterm-dashboard--host-at-point ()
  "Return dashboard host at point, or nil."
  (tterm-dashboard--line-property-at-point 'tterm-host))

(defun tterm-dashboard--goto-terminal-id (id)
  "Move point to dashboard row for terminal ID.
Return non-nil when such a row exists."
  (let ((pos (point-min))
        found)
    (while (and (< pos (point-max)) (not found))
      (when (equal (get-text-property pos 'tterm-terminal-id) id)
        (setq found pos))
      (setq pos (or (next-single-property-change pos 'tterm-terminal-id nil
                                                 (point-max))
                    (point-max))))
    (when found
      (goto-char found)
      t)))

(defun tterm-dashboard-select-window (terminal-id)
  "Select the live terminal buffer identified by TERMINAL-ID."
  (let ((buffer (tterm--buffer-for-terminal-id terminal-id)))
    (unless buffer (user-error "Terminal buffer has closed"))
    (with-current-buffer buffer (setq-local tterm--unread-notifications 0))
    (switch-to-buffer buffer)
    (tterm--attention-refresh)))

(defun tterm-dashboard-select ()
  "Select the tterm buffer on the current dashboard row."
  (interactive)
  (let ((id (tterm-dashboard--terminal-id-at-point)))
    (unless id
      (user-error "No tterm window on this line"))
    (tterm-dashboard-select-window id)))

(defun tterm-dashboard-detach-window ()
  "Detach the attached tterm window on the current dashboard row.
Disposes the attached tterm buffer to stop its redraw timers."
  (interactive)
  (let ((id (tterm-dashboard--terminal-id-at-point)))
    (unless id
      (user-error "No attached tterm window on this line"))
    (tterm-bridge-close id)
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
      (tterm-bridge-close id))
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
         (id (tterm--start rows cols host
                             (tterm--normalize-start-cwd cwd))))
    (tterm--attach-terminal-buffer id rows cols host cwd)))

(defun tterm-dashboard-next (&optional count)
  "Move to the next tterm window row.
With COUNT, move that many rows."
  (interactive "p")
  (dotimes (_ (or count 1))
    (let ((current-id (tterm-dashboard--terminal-id-at-point))
          (pos (point))
          next)
      (while (and (< pos (point-max)) (not next))
        (setq pos (or (next-single-property-change
                       pos 'tterm-terminal-id nil (point-max))
                      (point-max)))
        (let ((id (get-text-property pos 'tterm-terminal-id)))
          (when (and id (not (equal id current-id)))
            (setq next pos))))
      (if next
          (tterm-dashboard--goto-terminal-id
           (get-text-property next 'tterm-terminal-id))
        (user-error "No next tterm window")))))

(defun tterm-dashboard-previous (&optional count)
  "Move to the previous tterm window row.
With COUNT, move that many rows."
  (interactive "p")
  (dotimes (_ (or count 1))
    (let ((current-id (tterm-dashboard--terminal-id-at-point))
          (pos (point-min))
          previous-id)
      (while (< pos (point))
        (let ((id (get-text-property pos 'tterm-terminal-id)))
          (when (and id
                     (not (equal id current-id))
                     (not (equal id previous-id)))
            (setq previous-id id)))
        (setq pos (or (next-single-property-change
                       pos 'tterm-terminal-id nil (point))
                      (point))))
      (if previous-id
          (tterm-dashboard--goto-terminal-id previous-id)
        (user-error "No previous tterm window")))))

(defun tterm-dashboard--window-action (button)
  "Handle dashboard window BUTTON activation."
  (tterm-dashboard-select-window (button-get button 'tterm-terminal-id)))

(defun tterm-dashboard--insert-window (window)
  "Insert one dashboard WINDOW row."
  (let* ((id (plist-get window :terminal-id))
         (name (or (plist-get window :name) (number-to-string id)))
         (cwd (plist-get window :cwd))
         (status (plist-get window :status))
         (notification (plist-get window :notification))
         (row-props (list 'tterm-terminal-id id
                          'tterm-host (plist-get window :host)
                          'mouse-face 'highlight)))
    (let ((row-start (point)))
      (insert "  ")
      (insert-text-button name
                          'action #'tterm-dashboard--window-action
                          'follow-link t)
      (insert (format "  %d" id))
      (when status
        (insert (propertize
                 (format "  [%s]" (pcase status
                                     (`(exited ,code) (format "exited %d" code))
                                     (_ (symbol-name status))))
                 'face 'bold)))
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
        (insert (propertize "No tterm processes\n" 'face 'shadow))
      (dolist (host snapshot)
        (let ((host-start (point))
              (host-name (plist-get host :host)))
          (insert (propertize host-name 'face 'bold) "\n")
          (add-text-properties host-start (point) (list 'tterm-host host-name))
          (if-let* ((windows (plist-get host :windows)))
              (dolist (window windows)
                (tterm-dashboard--insert-window window))
            (let ((empty-start (point)))
              (insert (propertize "  No terminal buffers\n" 'face 'shadow))
              (add-text-properties empty-start (point)
                                   (list 'tterm-host host-name)))))
        (insert "\n"))))
  (goto-char (or (tterm-dashboard--window-position-at-or-after (point-min))
                 (point-min))))

(defun tterm-dashboard--refresh ()
  "Refresh the dashboard from Lisp's live process buffers."
  (let ((id (tterm-dashboard--terminal-id-at-point))
        (snapshot (tterm--process-snapshot)))
    (unless (equal snapshot tterm-dashboard--last-snapshot)
      (setq tterm-dashboard--last-snapshot snapshot)
      (tterm-dashboard--render snapshot)
      (when id (tterm-dashboard--goto-terminal-id id)))))

;;;###autoload
(defun tterm-dashboard-refresh ()
  "Refresh the tterm dashboard buffer.
When called from a non-dashboard buffer, target
`tterm-dashboard-buffer-name' instead of the current buffer."
  (interactive)
  (if (derived-mode-p 'tterm-dashboard-mode)
      (tterm-dashboard--refresh)
    (let ((buffer (get-buffer-create tterm-dashboard-buffer-name)))
      (with-current-buffer buffer
        (unless (derived-mode-p 'tterm-dashboard-mode)
          (tterm-dashboard-mode))
        (tterm-dashboard--refresh)))))

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
