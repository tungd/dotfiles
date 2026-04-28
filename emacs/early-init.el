;;; early-init.el --- Early startup configuration -*- lexical-binding: t; -*-

(defun td/early-frame-parameter (parameter value)
  "Set PARAMETER to VALUE for the initial and future frames."
  (setq initial-frame-alist
        (cons (cons parameter value)
              (assq-delete-all parameter initial-frame-alist))
        default-frame-alist
        (cons (cons parameter value)
              (assq-delete-all parameter default-frame-alist))))

(td/early-frame-parameter 'tool-bar-lines 0)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;;; early-init.el ends here
