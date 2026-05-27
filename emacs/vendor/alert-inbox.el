;;; alert-inbox.el --- Inbox style for alert.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'alert)

(defgroup alert-inbox nil
  "Inbox and navigation commands for alert.el notifications."
  :group 'alert)

(defcustom alert-inbox-max-items 200
  "Maximum number of alert records kept in `alert-inbox'."
  :type 'integer
  :group 'alert-inbox)

(cl-defstruct (alert-inbox-entry
               (:constructor alert-inbox--make-entry))
  id
  time
  message
  title
  category
  severity
  mode
  buffer
  data)

(defvar alert-inbox--entries nil
  "Alert inbox records, newest first.")

(defvar alert-inbox--next-id 0
  "Next alert inbox record id.")

(defvar alert-inbox-mode nil
  "Non-nil when alert inbox mode is enabled.")

(defvar alert-inbox--mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'alert-inbox-cycle-buffer)
    (define-key map [mode-line mouse-2] #'alert-inbox-open)
    map)
  "Keymap for the alert inbox mode-line indicator.")

(defvar alert-inbox--mode-line-form
  '(:eval (alert-inbox--mode-line-string))
  "Mode-line form installed by `alert-inbox-mode'.")

(defvar alert-inbox-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'alert-inbox-refresh)
    (define-key map (kbd "RET") #'alert-inbox-open-entry)
    (define-key map (kbd "d") #'alert-inbox-delete-entry)
    (define-key map (kbd "k") #'alert-inbox-delete-entry)
    (define-key map (kbd "x") #'alert-inbox-clear)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `alert-inbox-list-mode'.")

(defun alert-inbox--entry-live-p (entry)
  "Return non-nil when ENTRY's source buffer is live."
  (let ((buffer (alert-inbox-entry-buffer entry)))
    (and (bufferp buffer) (buffer-live-p buffer))))

(defun alert-inbox--trim ()
  "Trim `alert-inbox--entries' to `alert-inbox-max-items'."
  (cond
   ((<= alert-inbox-max-items 0)
    (setq alert-inbox--entries nil))
   ((> (length alert-inbox--entries) alert-inbox-max-items)
    (setcdr (nthcdr (1- alert-inbox-max-items) alert-inbox--entries) nil))))

(defun alert-inbox--changed ()
  "Refresh alert inbox displays after a state change."
  (force-mode-line-update t)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'alert-inbox-list-mode)
        (alert-inbox-refresh)))))

(defun alert-inbox-record (info)
  "Record alert INFO in the alert inbox."
  (setq alert-inbox--next-id (1+ alert-inbox--next-id))
  (push (alert-inbox--make-entry
         :id alert-inbox--next-id
         :time (current-time)
         :message (plist-get info :message)
         :title (plist-get info :title)
         :category (plist-get info :category)
         :severity (plist-get info :severity)
         :mode (plist-get info :mode)
         :buffer (plist-get info :buffer)
         :data (plist-get info :data))
        alert-inbox--entries)
  (alert-inbox--trim)
  (alert-inbox--changed))

(defun alert-inbox-notify (info)
  "Notifier function for the `inbox' alert style."
  (alert-inbox-record info))

(alert-define-style 'inbox
                    :title "Record in alert inbox"
                    :notifier #'alert-inbox-notify
                    :remover #'ignore)

(defun alert-inbox--source-buffers ()
  "Return live source buffers with alert inbox entries, newest first."
  (let (buffers)
    (dolist (entry alert-inbox--entries)
      (let ((buffer (alert-inbox-entry-buffer entry)))
        (when (and (bufferp buffer)
                   (buffer-live-p buffer)
                   (not (memq buffer buffers)))
          (push buffer buffers))))
    (nreverse buffers)))

;;;###autoload
(defun alert-inbox-cycle-buffer ()
  "Switch to the next source buffer represented in the alert inbox."
  (interactive)
  (let ((buffers (alert-inbox--source-buffers)))
    (if buffers
        (let* ((current (current-buffer))
               (next (or (cadr (memq current buffers))
                         (car buffers))))
          (switch-to-buffer next))
      (message "No alert inbox source buffers"))))

(defun alert-inbox--format-value (value)
  "Format VALUE for compact alert inbox display."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   (t (format "%S" value))))

(defun alert-inbox--entry-line (entry)
  "Return the first display line for ENTRY."
  (format "%s  #%d  %s  %s  %s"
          (format-time-string "%Y-%m-%d %H:%M:%S"
                              (alert-inbox-entry-time entry))
          (alert-inbox-entry-id entry)
          (alert-inbox--format-value (alert-inbox-entry-severity entry))
          (alert-inbox--format-value (alert-inbox-entry-category entry))
          (if (alert-inbox--entry-live-p entry)
              (buffer-name (alert-inbox-entry-buffer entry))
            "<dead buffer>")))

(defun alert-inbox--insert-entry (entry)
  "Insert ENTRY at point."
  (let ((start (point)))
    (insert (alert-inbox--entry-line entry) "\n")
    (when-let* ((title (alert-inbox-entry-title entry)))
      (insert title "\n"))
    (when-let* ((message (alert-inbox-entry-message entry)))
      (insert message "\n"))
    (insert "\n")
    (add-text-properties start (point)
                         `(alert-inbox-entry-id
                           ,(alert-inbox-entry-id entry)))))

;;;###autoload
(defun alert-inbox-refresh ()
  "Refresh the current alert inbox buffer."
  (interactive)
  (unless (eq major-mode 'alert-inbox-list-mode)
    (user-error "Not in an alert inbox buffer"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (if alert-inbox--entries
        (dolist (entry alert-inbox--entries)
          (alert-inbox--insert-entry entry))
      (insert "No alerts.\n"))
    (goto-char (point-min))))

(define-derived-mode alert-inbox-list-mode special-mode "Alert-Inbox"
  "Major mode for reviewing alert inbox entries."
  (setq-local truncate-lines nil))

;;;###autoload
(defun alert-inbox-open ()
  "Open the alert inbox buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Alert Inbox*")))
    (with-current-buffer buffer
      (alert-inbox-list-mode)
      (alert-inbox-refresh))
    (pop-to-buffer buffer)))

(defun alert-inbox--entry-at-point ()
  "Return the alert inbox entry at point."
  (let ((id (get-text-property (point) 'alert-inbox-entry-id)))
    (unless id
      (user-error "No alert at point"))
    (or (cl-find id alert-inbox--entries
                 :key #'alert-inbox-entry-id
                 :test #'=)
        (user-error "Alert is no longer present"))))

;;;###autoload
(defun alert-inbox-open-entry ()
  "Open the source buffer for the alert inbox entry at point."
  (interactive)
  (let* ((entry (alert-inbox--entry-at-point))
         (buffer (alert-inbox-entry-buffer entry)))
    (unless (and (bufferp buffer) (buffer-live-p buffer))
      (user-error "Alert source buffer is gone"))
    (switch-to-buffer buffer)))

;;;###autoload
(defun alert-inbox-delete-entry ()
  "Delete the alert inbox entry at point."
  (interactive)
  (let ((entry (alert-inbox--entry-at-point)))
    (setq alert-inbox--entries
          (delq entry alert-inbox--entries))
    (alert-inbox--changed)))

;;;###autoload
(defun alert-inbox-clear ()
  "Clear all alert inbox entries."
  (interactive)
  (setq alert-inbox--entries nil)
  (alert-inbox--changed))

(defun alert-inbox--mode-line-string ()
  "Return alert inbox mode-line indicator."
  (when (and alert-inbox-mode alert-inbox--entries)
    (let ((text (format " [A:%d]" (length alert-inbox--entries))))
      (add-text-properties
       0 (length text)
       `(local-map ,alert-inbox--mode-line-map
         mouse-face mode-line-highlight
         help-echo "mouse-1: cycle alert source buffers; mouse-2: open alert inbox")
       text)
      text)))

;;;###autoload
(define-minor-mode alert-inbox-mode
  "Toggle alert inbox mode-line integration."
  :global t
  :group 'alert-inbox
  :lighter nil
  (if alert-inbox-mode
      (progn
        (unless (listp global-mode-string)
          (setq global-mode-string (list global-mode-string)))
        (unless (member alert-inbox--mode-line-form global-mode-string)
          (setq global-mode-string
                (append global-mode-string
                        (list alert-inbox--mode-line-form))))
        (force-mode-line-update t))
    (setq global-mode-string
          (remove alert-inbox--mode-line-form global-mode-string))
    (force-mode-line-update t)))

(provide 'alert-inbox)

;;; alert-inbox.el ends here
