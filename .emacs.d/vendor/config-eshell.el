
(use-package eshell
  :defer t
  :config
  (progn
    (defmacro td/with-face (str &rest properties)
      `(propertize ,str 'face (list ,@properties)))

    (defun td/eshell-pwd ()
      (replace-regexp-in-string
       (regexp-quote (expand-file-name "~"))
       "~"
       (eshell/pwd)))

    (defun td/eshell-prompt ()
      (format
       "\n%s@%s in %s\n%s "
       (td/with-face user-login-name :foreground "#dc322f")
       (td/with-face (or (getenv "HOST") (system-name)) :foreground "#b58900")
       (td/with-face (td/eshell-pwd) :foreground "#859900")
       (if (= (user-uid) 0) (with-face "#" :foreground "red") "$")))

    (defalias 'eshell/e 'find-file-other-window)

    (defun eshell/open (args)
      (interactive)
      (shell-command
       (concat (case system-type
                 ((darwin) "open")
                 ((windows-nt) "start")
                 (t "xdg-open"))
               (format " %s" args))))

    (use-package em-prompt
      :defer t
      :config
      (setq eshell-prompt-function #'td/eshell-prompt
            eshell-prompt-regexp "^[^#$\\n]*[#$] "
            eshell-highlight-prompt nil))))
