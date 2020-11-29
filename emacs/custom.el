(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-safe-themes
   '("7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "16ce45f31cdea5e74ca4d27519d7ebe998d69ec3bf7df7be63c5ffdb5638b387" default))
 '(fci-rule-color "#7c6f64")
 '(jdee-db-active-breakpoint-face-colors (cons "#0d1011" "#fabd2f"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#0d1011" "#b8bb26"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#0d1011" "#928374"))
 '(objed-cursor-color "#fb4934")
 '(package-selected-packages
   '(poetry ob-graphql rainbow-delimiters ag yasnippet yaml-mode window-numbering which-key web-mode visual-regexp use-package undo-tree typescript-mode twilight-theme tommyh-theme terraform-doc tangotango-theme tango-plus-theme sublime-themes smartparens selectrum-prescient rust-mode rainbow-mode py-isort paper-theme ob-http nginx-mode mustang-theme magit lsp-pyright lsp-haskell inf-clojure highlight-numbers hideshow-org greymatters-theme golden-ratio github-theme fsharp-mode feature-mode expand-region exec-path-from-shell eshell-z eshell-up eshell-toggle eshell-git-prompt emmet-mode doom-themes dockerfile-mode docker delight ctrlf crux company-terraform company-prescient comment-dwim-2 cloud-theme badwolf-theme))
 '(pdf-view-midnight-colors (cons "#ebdbb2" "#282828"))
 '(rustic-ansi-faces
   ["#282828" "#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#cc241d" "#8ec07c" "#ebdbb2"])
 '(safe-local-variable-values
   '((python-shell-interpreter . "poetry run outsight/manage.py shell")
     (eval add-hook 'after-save-hook
           (lambda nil
             (org-babel-tangle)
             (byte-recompile-file "~/.config/emacs/init.el"))
           nil t)))
 '(show-paren-mode t)
 '(vc-annotate-background "#282828")
 '(vc-annotate-color-map
   (list
    (cons 20 "#b8bb26")
    (cons 40 "#cebb29")
    (cons 60 "#e3bc2c")
    (cons 80 "#fabd2f")
    (cons 100 "#fba827")
    (cons 120 "#fc9420")
    (cons 140 "#fe8019")
    (cons 160 "#ed611a")
    (cons 180 "#dc421b")
    (cons 200 "#cc241d")
    (cons 220 "#db3024")
    (cons 240 "#eb3c2c")
    (cons 260 "#fb4934")
    (cons 280 "#e05744")
    (cons 300 "#c66554")
    (cons 320 "#ac7464")
    (cons 340 "#7c6f64")
    (cons 360 "#7c6f64")))
 '(vc-annotate-very-old-color nil))
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
