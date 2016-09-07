
(use-package company
  :ensure t
  :defer t
  :bind ("M-/" . company-complete-common-or-cycle)
  :init (global-company-mode t)
  :config
  (progn
    (setq company-minimum-prefix-length 2
          ;; The basic idea is preview completion inline immediately,
          ;; and then wait a little bit to show all the available completions
          company-idle-delay 0.2
          company-echo-delay 0.8
          company-tooltip-idle-delay 0.8
          company-tooltip-align-annotations t
          company-tooltip-limit 8
          company-dabbrev-downcase nil
          company-dabbrev-ignore-case t
          company-frontends
          '(company-pseudo-tooltip-unless-just-one-frontend-with-delay
            company-preview-frontend
            company-echo-metadata-frontend)
          company-backends
          '((company-yasnippet
             company-dabbrev
             company-capf)))

    (set-face-attribute 'company-preview nil
                        :underline t
                        :foreground (face-foreground 'font-lock-comment-face))

    (bind-keys :map company-active-map
               ("<tab>" . company-complete-common-or-cycle)
               ("C-n" . company-select-next-or-abort)
               ("C-p" . company-select-previous-or-abort))))

(use-package company-web
  :ensure t
  :defer t
  :init
  (eval-after-load 'web-mode
    '(add-to-list 'company-backends 'company-web-html)))

(use-package company-statistics
  :ensure t
  :defer t
  :init (company-statistics-mode t))
