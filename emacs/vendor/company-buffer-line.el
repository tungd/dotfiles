
(require 'cl-lib)
(require 'company)
(require 's)
(require 'seq)

(defun company-buffer-line--buffer-lines (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (mapcar #'s-trim
              (split-string (buffer-string) "[\n\r]")))))

(defun company-buffer-line--same-mode-buffers (&optional mode)
  (let ((mode (or mode major-mode)))
    (cl-loop for buffer in (buffer-list)
             if (with-current-buffer buffer
                  (eq major-mode mode))
             collect buffer)))

(defun company-current-buffer-lines (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-current-buffer-lines))
    (prefix (when (looking-back "^\s*\\(.+\\)" (line-beginning-position))
              (match-string-no-properties 1)))
    (candidates (all-completions arg (company-buffer-line--buffer-lines)))
    (sorted t)))


(defun company-same-mode-buffer-lines (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-same-mode-buffer-lines))
    (prefix (when (looking-back "^\s*\\(.+\\)" (line-beginning-position))
              (match-string-no-properties 1)))
    (candidates (all-completions
                 arg
                 (seq-mapcat #'company-buffer-line--buffer-lines
                             (company-buffer-line--same-mode-buffers))))
    (sorted t)))

(provide 'company-buffer-line)
