(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "e47e52c3dac4c3b6a77e32dcdee6de63858277247485f7c569b35c04de9a1501" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" default))
 '(package-selected-packages
   '(multi-vterm vterm-toggle doom-themes golden-ratio ein clojure-inf clojure clojure-mode inf-clojure kotlin-mode feature-mode company-box company-prescient typescript-mode mini-modeline vterm popup-complete which-key completions-frame poetry fancy-dabbrev smartparens selectrum-prescient prescient selectrum deadgrep eshell-git-prompt tango-plus-theme highlight-numbers ob-http yaml-mode nginx-mode company-terraform terraform-doc terraform-mode dockerfile-mode docker rust-mode lsp-haskell haskell-mode py-isort rainbow-mode emmet-mode web-mode magit with-editor eshell-z eshell-up eshell-toggle yasnippet undo-tree expand-region ctrlf visual-regexp crux comment-dwim-2 window-numbering exec-path-from-shell use-package delight))
 '(recentf-mode t)
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
           (lambda nil
             (org-babel-tangle)
             (byte-recompile-file "~/.config/emacs/init.el"))
           nil t)))
 '(vc-annotate-very-old-color nil))
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
 '(indent-guide-face ((t (:inherit font-lock-comment-face))))
 '(line-number ((t :inherit font-lock-comment-face)))
 '(markdown-inline-code-face ((t (:slant normal))))
 '(vertical-border ((t (:foreground "#000" :background "#000"))))
 '(web-mode-variable-name-face ((t (:inherit default)))))
