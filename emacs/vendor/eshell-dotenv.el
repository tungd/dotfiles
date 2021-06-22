(require 'project)
(require 'em-dirs)

;; TODO: handle multiple eshell
(defvar eshell-dotenv--current nil)
(defvar eshell-dotenv--vars nil)

(defun eshell-dotenv--apply (dotenv-path)
  (when (file-exists-p dotenv-path)
    (with-temp-buffer
      (insert-file-contents dotenv-path)
      (goto-char (point-min))
      (while (not (eobp))
        (search-forward "=")
        (let* ((key (buffer-substring (line-beginning-position) (- (point) 1)))
               (value (buffer-substring (point) (line-end-position)))
               (current (getenv key)))
          (add-to-list 'eshell-dotenv--vars (list key current))
          (setenv key value))
        (forward-line 1)))
    (setq eshell-dotenv--current dotenv-path)))

(defun eshell-dotenv--restore ()
  (cl-loop
   for v in eshell-dotenv--vars
   do (setenv (car v) (cdr v)))
  (setq eshell-dotenv--vars nil)
  (setq eshell-dotenv--current nil))

;;;###autoload
(defun eshell-dotenv-update ()
  (when-let ((root (project-root (project-current))))
    (when (or (not eshell-dotenv--current)
              (not root)
              (not (string-equal (expand-file-name ".env" root) eshell-dotenv--current)))
      (message "%s %s" root eshell-dotenv--current)
      (eshell-dotenv--restore)
      (eshell-dotenv--apply (expand-file-name ".env" root)))))

(provide 'eshell-dotenv)
