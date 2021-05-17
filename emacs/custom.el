(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1d78d6d05d98ad5b95205670fe6022d15dabf8d131fe087752cc55df03d88595" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(flycheck-keymap-prefix (kbd "C-c e"))
 '(mini-frame-show-paramters '((top . 0) (width . 90) (left . 0)))
 '(package-selected-packages
   '(mini-frame flycheck github-theme smart-mode-line yasnippet yaml-mode which-key web-mode visual-regexp use-package typescript-mode tuareg terraform-mode terraform-doc tango-plus-theme sqlformat smartparens selectrum-prescient reason-mode rainbow-mode py-isort ob-http ob-graphql nginx-mode magit lsp-pyright highlight-numbers haskell-mode golden-ratio feature-mode expand-region exec-path-from-shell evil-surround eshell-z eshell-up eshell-toggle emmet-mode dune dockerfile-mode docker delight ctrlf crux consult company-prescient comment-dwim-2 ag ace-window))
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
           (lambda nil
             (org-babel-tangle)
             (byte-recompile-file "~/.config/emacs/init.el"))
           nil t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:inherit nil :background nil :foreground "#deae3e"))))
 '(diff-hl-delete ((t (:inherit nil :background nil :foreground "#ff0000"))))
 '(diff-hl-insert ((t (:inherit nil :background nil :foreground "#81af34"))))
 '(font-lock-comment-delimiter-face ((t (:slant normal))))
 '(font-lock-comment-face ((t (:slant normal))))
 '(font-lock-constant-face ((t (:slant normal))))
 '(font-lock-string-face ((t (:slant normal))))
 '(fringe ((t (:background nil))))
 '(hl-line ((t :foreground nil :background "#323850")))
 '(indent-guide-face ((t (:inherit font-lock-comment-face))))
 '(line-number ((t :foreground nil :inherit font-lock-comment-face)))
 '(markdown-inline-code-face ((t (:slant normal))))
 '(vertical-border ((t (:foreground "#000" :background "#000"))))
 '(web-mode-variable-name-face ((t (:inherit default)))))
