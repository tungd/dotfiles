
(use-package evil
  :ensure t
  :defer t
  ;; :init (evil-mode t)
  :config
  (progn
    (setq evil-ex-substitute-global t
          evil-cross-lines t
          evil-move-cursor-back t)

    (use-package evil-surround
      :ensure t
      :defer t
      :init (global-evil-surround-mode t))

    (use-package evil-visualstar
      :ensure t
      :defer t
      :init (global-evil-visualstar-mode))

    (use-package evil-org
      :ensure t)

    (bind-keys :map evil-insert-state-map
               ([remap newline] . newline-and-indent))

    (bind-keys :map evil-normal-state-map
               ("TAB" . evil-jump-item)
               ("<tab>" . evil-jump-item)
               ("j" . evil-next-visual-line)
               ("k" . evil-previous-visual-line)
               ("M-j" . td/evil-next-ten-visual-line)
               ("M-k" . td/evil-previous-ten-visual-line))

    (bind-keys :map evil-motion-state-map
               ("TAB" . evil-jump-item)
               ("<tab>" . evil-jump-item))

    (defun td/evil-next-ten-visual-line ()
      (interactive)
      (evil-next-visual-line 10))

    (defun td/evil-previous-ten-visual-line ()
      (interactive)
      (evil-previous-visual-line 10))

    (defun td/open-line ()
      (interactive)
      (end-of-line)
      (newline-and-indent))

    (defun td/ends-with-colon ()
      (interactive)
      (end-of-line)
      (insert ":"))

    (defun td/ends-with-semicolon ()
      (interactive)
      (end-of-line)
      (insert ";"))

    (bind-keys :map evil-insert-state-map
               ("C-e" . end-of-line)
               ((kbd "<C-return>") . td/open-line)
               ("C-;" . td/ends-with-semicolon)
               ("C-:" . td/ends-with-colon))))
