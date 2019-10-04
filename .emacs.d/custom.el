(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("071f5702a5445970105be9456a48423a87b8b9cfa4b1f76d15699b29123fb7d8" default))
 '(datetime-timezone 'Asia/Ho_Chi_Minh)
 '(package-selected-packages
   '(docker haskell-mode dune utop eldoc-box eldoc-overlay expand-region ivy-posframe use-package anzu tuareg yasnippet yaml-mode window-numbering web-mode undo-tree tango-plus-theme swift-mode solidity-mode smex smartparens smart-mode-line rust-mode request rainbow-mode rainbow-delimiters pyvenv py-isort ob-sql-mode ob-http nginx-mode markdown-mode magit kotlin-mode ivy-historian indent-guide highlight-numbers groovy-mode go-mode exec-path-from-shell emmet-mode eglot dumb-jump doom-themes dockerfile-mode diminish diff-hl crux counsel company-statistics comment-dwim-2 ag add-node-modules-path 0blayout))
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
           (lambda nil
             (org-babel-tangle)
             (byte-compile-file "~/.emacs.d/init.el"))
           nil t)
     (eval add-hook 'after-save-hook
           (lambda nil
             (org-babel-tangle)
             (async-byte-compile-file "~/.emacs.d/init.el"))
           nil t)
     (eval add-hook 'after-save-hook
           (lambda nil
             (org-babel-tangle)
             (byte-recompile-file "~/.emacs.d/init.el"))
           nil t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:inherit nil :background nil :foreground "#deae3e"))))
 '(diff-hl-delete ((t (:inherit nil :background nil :foreground "#ff0000"))))
 '(diff-hl-insert ((t (:inherit nil :background nil :foreground "#81af34"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground nil :slant normal))))
 '(font-lock-comment-face ((t (:slant normal))))
 '(font-lock-constant-face ((t (:slant normal))))
 '(font-lock-keyword-face ((t (:weight normal))))
 '(font-lock-string-face ((t (:slant normal))))
 '(indent-guide-face ((t (:inherit font-lock-comment-face))))
 '(line-number ((t (:height 120))))
 '(line-number-current-line ((t (:height 120 :bold nil))))
 '(nix-attribute-face ((t (:inherit font-lock-builtin-face))))
 '(rainbow-delimiters-unmatched-face ((t (:inherit error :background "#f00"))))
 '(vertical-border ((t (:foreground "#000" :background "#000"))))
 '(web-mode-variable-name-face ((t (:inherit default)))))
