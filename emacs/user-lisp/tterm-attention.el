;;; tterm-attention.el --- Optional cross-buffer notifications -*- lexical-binding: t; -*-
(require 'tterm)

(defcustom tterm-attention-refresh-interval 2.0
  "Seconds between Lisp notification refreshes while tterm buffers exist."
  :type 'number
  :group 'tterm)

(defvar tterm--attention-refresh-timer nil
  "Timer that refreshes global tterm attention state.")

(defvar tterm--attention-unread-total 0
  "Total unread terminal notifications across backend windows.")

(defvar tterm--attention-windows nil
  "Dashboard window plists with unread terminal notifications.")

(defvar tterm--attention-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'tterm-jump-next-notification)
    map)
  "Mode-line keymap for tterm attention indicator.")

;;; Cross-pane attention

(declare-function tterm-dashboard-select-window
                  "tterm-dashboard" (terminal-id))

(defun tterm--attention-window-unread-p (window)
  "Return non-nil when dashboard WINDOW has unread notifications."
  (> (or (plist-get window :unread-notifications) 0) 0))

(defun tterm--attention-windows-from-snapshot (snapshot)
  "Return unread dashboard windows from decoded SNAPSHOT."
  (let (windows)
    (dolist (group snapshot)
      (dolist (window (plist-get group :windows))
        (when (tterm--attention-window-unread-p window)
          (push window windows))))
    (nreverse windows)))

(defun tterm--attention-apply-snapshot (snapshot)
  "Update cached attention state from decoded dashboard SNAPSHOT.
Only refresh mode lines when the attention state actually changed, and
restrict the refresh to live tterm buffers (LOOP 6.2). The previous code
called `(force-mode-line-update t)` unconditionally, invalidating every
window's mode line in every frame every 2s even when nothing changed."
  (let* ((windows (tterm--attention-windows-from-snapshot snapshot))
         (unread (apply #'+
                        (mapcar (lambda (window)
                                  (or (plist-get window :unread-notifications) 0))
                                windows))))
    (unless (and (equal windows tterm--attention-windows)
                 (eq unread tterm--attention-unread-total))
      (setq tterm--attention-windows windows)
      (setq tterm--attention-unread-total unread)
      (dolist (buffer (tterm--buffers))
        (with-current-buffer buffer
          (force-mode-line-update))))))

(defun tterm--attention-stop-timer ()
  "Stop the attention refresh timer and clear cached state."
  (when (timerp tterm--attention-refresh-timer)
    (cancel-timer tterm--attention-refresh-timer))
  (setq tterm--attention-refresh-timer nil)
  (setq tterm--attention-unread-total 0)
  (setq tterm--attention-windows nil)
  (force-mode-line-update t))

(defun tterm--process-snapshot ()
  "Return dashboard groups derived from live Lisp terminal buffers."
  (let (groups)
    (dolist (buffer (tterm--buffers))
      (with-current-buffer buffer
        (when tterm--terminal
          (let* ((id (tterm-id tterm--terminal))
                 (host (or (tterm-host tterm--terminal) "local"))
                 (group (or (cl-find host groups :key (lambda (g) (plist-get g :host)) :test #'equal)
                            (let ((g (list :host host :windows nil)))
                              (push g groups) g)))
                 (window (list :terminal-id id :host host
                               :name (or (tterm-title tterm--terminal) (buffer-name))
                               :cwd (tterm-cwd tterm--terminal)
                               :status (tterm-status tterm--terminal)
                               :notification tterm--latest-notification
                               :unread-notifications tterm--unread-notifications)))
            (plist-put group :windows (append (plist-get group :windows) (list window)))))))
    (nreverse groups)))

(defun tterm--attention-refresh ()
  "Refresh attention from buffer-owned notification state."
  (if (null (tterm--buffers))
      (tterm--attention-stop-timer)
    (tterm--attention-apply-snapshot (tterm--process-snapshot))))

(defun tterm--attention-ensure-timer ()
  "Ensure the attention refresh timer is running."
  (unless (timerp tterm--attention-refresh-timer)
    (setq tterm--attention-refresh-timer
          (run-at-time 0 tterm-attention-refresh-interval
                       #'tterm--attention-refresh))))

(defun tterm--attention-maybe-stop-later ()
  "Stop attention polling after the last tterm buffer is gone."
  (run-at-time 0 nil
               (lambda ()
                 (unless (tterm--buffers)
                   (tterm--attention-stop-timer)))))

(defun tterm--attention-setup-buffer ()
  "Set up global attention polling for a tterm buffer."
  (when (and (boundp 'tterm--terminal) tterm--terminal)
    (tterm--attention-ensure-timer))
  (add-hook 'kill-buffer-hook #'tterm--attention-maybe-stop-later nil t))

(defun tterm--attention-current-window-p (window)
  "Return non-nil when dashboard WINDOW is the current tterm buffer."
  (and tterm--terminal
       (eql (plist-get window :terminal-id) (tterm-id tterm--terminal))))

(defun tterm--attention-target-window ()
  "Return the unread dashboard window to jump to."
  (or (cl-find-if-not #'tterm--attention-current-window-p
                      tterm--attention-windows)
      (car tterm--attention-windows)))

(defun tterm--attention-window-summary (window)
  "Return a concise display summary for unread dashboard WINDOW."
  (let ((name (or (plist-get window :name)
                  (and (plist-get window :terminal-id)
                       (number-to-string (plist-get window :terminal-id)))
                  "tterm"))
        (notification (plist-get window :notification)))
    (if (and notification (not (string-empty-p notification)))
        (format "%s: %s" name notification)
      name)))

(defun tterm--mode-line-attention-indicator ()
  "Return mode-line text for global tterm unread attention."
  (if (<= tterm--attention-unread-total 0)
      ""
    (let ((text (format " [🔔:%d]" tterm--attention-unread-total)))
      (add-text-properties
       0 (length text)
       `(face tterm-notification-mode-line
         local-map ,tterm--attention-mode-line-map
         mouse-face mode-line-highlight
         help-echo "mouse-1 or C-c C-n: jump to tterm notification")
       text)
      text)))

(defun tterm--header-attention-indicator ()
  "Return header-line text for global tterm unread attention."
  (when (> tterm--attention-unread-total 0)
    (let ((summary (and tterm--attention-windows
                        (tterm--attention-window-summary
                         (car tterm--attention-windows)))))
      (if summary
          (format "attention: %s" summary)
        (format "attention: %d" tterm--attention-unread-total)))))

;;;###autoload
(defun tterm-jump-next-notification ()
  "Jump to the next tterm pane with unread terminal notifications."
  (interactive)
  (tterm--attention-refresh)
  (let ((window (tterm--attention-target-window)))
    (unless window
      (user-error "No unread tterm notifications"))
    (require 'tterm-dashboard)
    (tterm-dashboard-select-window (plist-get window :terminal-id))
    (run-at-time 0.5 nil #'tterm--attention-refresh)))

(defvar tterm-attention-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") #'tterm-jump-next-notification)
    map))

;;;###autoload
(define-minor-mode tterm-attention-mode
  "Show and refresh notification summaries across terminal buffers."
  :global t :group 'tterm :keymap tterm-attention-mode-map
  (if tterm-attention-mode
      (progn
        (add-hook 'tterm-mode-hook #'tterm--attention-setup-buffer)
        (add-hook 'tterm-terminal-started-hook #'tterm--attention-setup-buffer)
        (add-hook 'tterm-mode-line-functions #'tterm--mode-line-attention-indicator)
        (add-hook 'tterm-header-line-functions #'tterm--header-attention-indicator)
        (dolist (buffer (tterm--buffers))
          (with-current-buffer buffer (tterm--attention-setup-buffer))))
    (remove-hook 'tterm-mode-hook #'tterm--attention-setup-buffer)
    (remove-hook 'tterm-terminal-started-hook #'tterm--attention-setup-buffer)
    (remove-hook 'tterm-mode-line-functions #'tterm--mode-line-attention-indicator)
    (remove-hook 'tterm-header-line-functions #'tterm--header-attention-indicator)
    (dolist (buffer (tterm--buffers))
      (with-current-buffer buffer
        (remove-hook 'kill-buffer-hook #'tterm--attention-maybe-stop-later t)))
    (tterm--attention-stop-timer)))

(provide 'tterm-attention)
;;; tterm-attention.el ends here
