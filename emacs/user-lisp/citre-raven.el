;;; citre-raven.el --- Raven-backed Citre tags generation -*- lexical-binding: t; -*-

(require 'citre)
(require 'citre-ctags)

(defgroup citre-raven nil
  "Raven-backed Citre tags generation."
  :group 'citre)

(defcustom citre-raven-program (expand-file-name "~/.local/bin/rv")
  "Raven executable used to generate Citre-compatible tags files."
  :type 'file
  :group 'citre-raven)

(defun citre-raven--tags-file (dir)
  "Return the Citre tags file path for DIR."
  (let ((dir (expand-file-name dir)))
    (pcase citre-default-create-tags-file-location
      ('global-cache (citre--tags-file-in-global-cache dir))
      (_ (expand-file-name "tags" dir)))))

(defun citre-raven--index (tagsfile dir &optional sync callback)
  "Run Raven indexing for DIR into TAGSFILE.
When SYNC is non-nil, wait for completion.  Run CALLBACK after a
successful update."
  (unless (file-executable-p citre-raven-program)
    (user-error "Raven executable not found: %s" citre-raven-program))
  (setq tagsfile (expand-file-name tagsfile))
  (setq dir (expand-file-name dir))
  (make-directory (file-name-directory tagsfile) t)
  (let* ((buffer (get-buffer-create "*raven-tags*"))
         (command (list citre-raven-program "index"
                        (concat "--output=" tagsfile)
                        dir)))
    (with-current-buffer buffer
      (erase-buffer))
    (if sync
        (let ((status (apply #'process-file (car command) nil buffer nil (cdr command))))
          (unless (zerop status)
            (user-error "rv index exits %s. See *raven-tags* buffer" status))
          (citre-clear-tags-file-cache)
          (when callback (funcall callback))
          (message "Updated %s" tagsfile)
          t)
      (make-process
       :name "raven-tags"
       :buffer buffer
       :command command
       :connection-type 'pipe
       :sentinel
       (lambda (proc _event)
         (when (eq (process-status proc) 'exit)
           (if (zerop (process-exit-status proc))
               (progn
                 (citre-clear-tags-file-cache)
                 (when callback (funcall callback))
                 (message "Updated %s" tagsfile))
             (message "rv index exits %s. See *raven-tags* buffer"
                      (process-exit-status proc))))))
      (message "Updating %s..." tagsfile)
      t)))

;;;###autoload
(defun citre-raven-create-tags-file (&optional sync)
  "Create a Citre-compatible tags file for the current project using Raven."
  (interactive "P")
  (let* ((dir (or (funcall citre-project-root-function)
                  (read-directory-name "Project root: " default-directory)))
         (tagsfile (citre-raven--tags-file dir)))
    (citre-raven--index tagsfile dir sync)))

;;;###autoload
(defun citre-raven-update-this-tags-file (&optional sync)
  "Update the current Citre tags file using Raven."
  (interactive "P")
  (let* ((dir (or (funcall citre-project-root-function) default-directory))
         (tagsfile (or (citre-tags-file-path)
                       (citre-raven--tags-file dir))))
    (citre-raven--index tagsfile dir sync)))

(provide 'citre-raven)

;;; citre-raven.el ends here
