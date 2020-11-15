(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rainbow-delimiters ag yasnippet yaml-mode window-numbering which-key web-mode visual-regexp use-package undo-tree typescript-mode twilight-theme tommyh-theme terraform-doc tangotango-theme tango-plus-theme sublime-themes smartparens selectrum-prescient rust-mode rainbow-mode py-isort paper-theme ob-http nginx-mode mustang-theme magit lsp-pyright lsp-haskell inf-clojure highlight-numbers hideshow-org greymatters-theme golden-ratio github-theme fsharp-mode feature-mode expand-region exec-path-from-shell eshell-z eshell-up eshell-toggle eshell-git-prompt emmet-mode doom-themes dockerfile-mode docker delight deadgrep ctrlf crux company-terraform company-prescient comment-dwim-2 cloud-theme badwolf-theme))
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
           (lambda nil
             (org-babel-tangle)
             (byte-recompile-file "~/.config/emacs/init.el"))
           nil t)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(diff-hl-change ((t (:inherit nil :background nil :foreground "#deae3e"))))
 '(diff-hl-delete ((t (:inherit nil :background nil :foreground "#ff0000"))))
 '(diff-hl-insert ((t (:inherit nil :background nil :foreground "#81af34"))))
 '(font-lock-comment-delimiter-face ((t (:slant normal))))
 '(font-lock-comment-face ((t (:slant normal))))
 '(font-lock-constant-face ((t (:slant normal))))
 '(font-lock-string-face ((t (:slant normal))))
 '(indent-guide-face ((t (:inherit font-lock-comment-face))))
 '(line-number ((t :foreground nil :inherit font-lock-comment-face)))
 '(markdown-inline-code-face ((t (:slant normal))))
 '(vertical-border ((t (:foreground "#000" :background "#000"))))
 '(web-mode-variable-name-face ((t (:inherit default)))))
