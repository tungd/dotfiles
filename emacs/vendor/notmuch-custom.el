;;; notmuch-custom.el --- Custom extensions for notmuch  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'notmuch)
(require 'subr-x)

;;; Custom Sync Function

(defun td/notmuch-sync ()
  "Sync email with mbsync, index with notmuch, and refresh the buffer."
  (interactive)
  (message "Syncing mail...")
  ;; Run mbsync and notmuch new in the background
  (start-process-shell-command
   "notmuch-sync"
   "*notmuch-sync*"
   ;; Combine commands: fetch -> index
   (format "mbsync -a && %s new" notmuch-command))

  ;; Set a sentinel to refresh the buffer once the process finishes
  (set-process-sentinel
   (get-process "notmuch-sync")
   (lambda (proc event)
     (when (string= event "finished\n")
       (message "Mail sync completed.")
       ;; Refresh the current notmuch buffer if it's open
       (when (and (eq major-mode 'notmuch-search-mode)
                  (get-buffer-window (current-buffer)))
         (notmuch-refresh-this-buffer))))))

(define-key notmuch-search-mode-map "S" 'td/notmuch-sync)
(define-key notmuch-hello-mode-map "S" 'td/notmuch-sync)

;;; HTML View in Xwidget

(defun td/find-first-html-part (parts)
  "Recursively search for a text/html part in a list of parts."
  (cl-loop for part in parts
           if (string= (plist-get part :content-type) "text/html")
           return part
           else if (string= (plist-get part :content-type) "multipart/alternative")
           return (td/find-first-html-part (plist-get part :content))
           else if (string= (plist-get part :content-type) "multipart/related")
           return (td/find-first-html-part (plist-get part :content))
           else if (string= (plist-get part :content-type) "multipart/mixed")
           return (td/find-first-html-part (plist-get part :content))))

(defun td/notmuch-show-view-html-xwidget ()
  "Find the HTML part, wrap it with a UTF-8 header, render in xwidget-webkit."
  (interactive)
  (let* ((msg (notmuch-show-get-message-properties))
         (body (plist-get msg :body))
         (html-part (td/find-first-html-part body)))

    (if html-part
        (let ((coding-system-for-write 'utf-8)
              (file (make-temp-file "notmuch-xwidget-" nil ".html")))

          ;; Fetch the content (notmuch usually handles QP/Base64 decoding automatically)
          (let ((content (notmuch-get-bodypart-text msg html-part notmuch-show-process-crypto)))

            (with-temp-file file
              ;; 1. Insert a proper HTML doctype and charset meta tag
              (insert "<!DOCTYPE html>\n")
              (insert "<html>\n<head>\n")
              (insert "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">\n")
              ;; Optional: Add viewport meta for better scaling
              (insert "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n")
              (insert "</head>\n<body>\n")

              ;; 2. Insert the actual email body
              (insert content)

              ;; 3. Close tags
              (insert "\n</body>\n</html>"))

            (xwidget-webkit-browse-url (concat "file://" file))
            (delete-other-windows)
            (message "Opened HTML view.")))
      (message "No HTML part found."))))

(define-key notmuch-show-mode-map "H" 'td/notmuch-show-view-html-xwidget)

;;; Automatic Maintenance

(defun td/trash-archived-inbox-mails ()
  "Physically trash emails that are in the Inbox folder but not tagged 'inbox'."
  (interactive)
  (let* ((mail-root (expand-file-name
                     (string-trim
                      (shell-command-to-string (concat notmuch-command " config get database.path")))))
         (accounts (cl-remove-if (lambda (dir) (member dir '("." ".." "drafts" ".notmuch")))
                                 (directory-files mail-root)))
         (files nil))
    (dolist (account accounts)
      (let* ((query (format "folder:%s/Inbox and not tag:inbox" account))
             (account-files (split-string (shell-command-to-string
                                           (format "%s search --output=files %s"
                                                   notmuch-command
                                                   (shell-quote-argument query)))
                                          "\n" t)))
        (setq files (append files account-files))))
    (if (not files)
        (unless (called-interactively-p 'interactive)
          (message "No archived inbox mails to trash."))
      (dolist (file files)
        (when (file-exists-p file)
          (move-file-to-trash file)))
      (shell-command (concat notmuch-command " new"))
      (message "Trashed %d archived inbox mails and updated index." (length files)))))

;; Add to midnight hook
(add-hook 'midnight-hook #'td/trash-archived-inbox-mails)

(provide 'notmuch-custom)
