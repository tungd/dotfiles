;;; tterm-consult.el --- Consult integration for tterm -*- lexical-binding: t; -*-

;; Optional Consult source for live tterm buffers.

(require 'tterm)

(defvar consult-buffer-sources)

(defvar tterm-consult-source
  '(:name "tterm"
    :narrow ?t
    :category buffer
    :items tterm--consult-buffer-items
    :annotate tterm--consult-buffer-annotate
    :action switch-to-buffer)
  "Consult source listing live `tterm-mode' buffers.")

(defun tterm--consult-buffer-items ()
  "Return tterm buffer names for `tterm-consult-source'."
  (mapcar
   (lambda (buffer)
     (propertize (buffer-name buffer) 'tterm-buffer buffer))
   (tterm--buffers)))

(defun tterm--consult-buffer-annotate (candidate)
  "Return Consult annotation text for tterm CANDIDATE."
  (let* ((buffer (get-text-property 0 'tterm-buffer candidate))
         (status (and (buffer-live-p buffer)
                      (tterm-osc-status-string buffer))))
    (if (and status (not (string-empty-p status)))
        (concat " " (propertize status 'face 'completions-annotations))
      "")))

;;;###autoload
(defun tterm-consult-register-source ()
  "Register `tterm-consult-source' with Consult when available."
  (interactive)
  (if (boundp 'consult-buffer-sources)
      (add-to-list 'consult-buffer-sources 'tterm-consult-source 'append)
    (user-error "Consult is not loaded")))

(with-eval-after-load 'consult
  (tterm-consult-register-source))

(provide 'tterm-consult)
;;; tterm-consult.el ends here
