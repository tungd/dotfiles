
(use-package evil
  :ensure t
  :defer t
  :init (evil-mode t)
  :config
  (progn
    (setq evil-ex-substitute-global t
          evil-cross-lines t)

    (add-to-list 'evil-emacs-state-modes 'diff-mode)

    (use-package evil-surround
      :ensure t
      :defer t
      ;; :commands (global-evil-surround-mode)
      :init (global-evil-surround-mode t))

    (use-package evil-visualstar
      :ensure t
      :defer t
      :init (global-evil-visualstar-mode))

    (use-package evil-org
      :ensure t)

    (use-package evil-snipe
      :ensure t
      :defer t
      :init
      (progn
        (evil-snipe-mode 1)
        (evil-snipe-override-mode 1))
      :config
      (progn
        (setq evil-snipe-spillover-scope 'visible)
        (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)))

    (bind-keys :map evil-insert-state-map
               ([remap newline] . newline-and-indent))

    (bind-keys :map evil-normal-state-map
               ("TAB" . evil-jump-item)
               ("<tab>" . evil-jump-item)
               ("j" . evil-next-visual-line)
               ("k" . evil-previous-visual-line)
               ("M-n" . td/next-ten-visual-line)
               ("M-p" . td/previous-ten-visual-line))

    (bind-keys :map evil-motion-state-map
               ("TAB" . evil-jump-item)
               ("<tab>" . evil-jump-item))

    (bind-keys :map evil-insert-state-map
               ("C-e" . end-of-line))))

(provide 'config-evil)
