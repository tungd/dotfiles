
(use-package vc-dir
  :defer t
  :config
  (progn
    (defun td/vc-git-command (verb fn)
      (let* ((args (vc-deduce-fileset nil t))
             (backend (car args))
             (files (nth 1 args)))
        (if (eq backend 'Git)
            (progn
              (funcall fn files)
              (message (concat verb " "
                               (number-to-string (length files))
                               " file(s).")))
          (message "Not in a vc git buffer."))))

    (defun td/vc-git-add (&optional revision args comment)
      (interactive "P")
      (td/vc-git-command "Staged" 'vc-git-register))

    (defun td/vc-git-reset (&optional revision args comment)
      (interactive "P")
      (td/vc-git-command
       "Unstaged"
       (lambda (files) (vc-git-command nil 0 files "reset" "-q" "--"))))

    (defun td/vc-git-amend (&optional revision args comment)
      (interactive "P")
      (td/vc-git-command
       "Ammended"
       (lambda (files)
         (vc-git-command nil 0 files
                         "commit" "--amend" "--reuse-message=HEAD"))))

    (defadvice vc-dir-refresh
        (after td/vc-hide-up-to-date-after-refresh activate)
      (vc-dir-hide-up-to-date))

    (bind-keys :map vc-dir-mode-map
               ("r" . vc-revert-buffer)
               ("a" . td/vc-git-add)
               ("u" . td/vc-git-reset)
               ("A" . td/vc-git-amend))

    (bind-keys :map vc-prefix-map
               ("r" . vc-revert-buffer)
               ("a" . td/vc-git-add)
               ("u" . td/vc-git-reset))))

(provide 'config-vc)
