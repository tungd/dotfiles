(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("45feb1f130c54e0fc116faa71c784562b41009ffc908cf5cef06b6df4bb60a9a" "d3856ef5a26c9f375f4a084af2e89fa215212fe44540deea941d264d00efead4" "0961d780bd14561c505986166d167606239af3e2c3117265c9377e9b8204bf96" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "6c3b5f4391572c4176908bb30eddc1718344b8eaff50e162e36f271f6de015ca" "6c9cbcdfd0e373dc30197c5059f79c25c07035ff5d0cc42aa045614d3919dab4" "3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "188fed85e53a774ae62e09ec95d58bb8f54932b3fd77223101d036e3564f9206" "c086fe46209696a2d01752c0216ed72fd6faeabaaaa40db9fc1518abebaf700d" "5036346b7b232c57f76e8fb72a9c0558174f87760113546d3a9838130f1cdb74" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" "cae81b048b8bccb7308cdcb4a91e085b3c959401e74a0f125e7c5b173b916bf9" "4a8d4375d90a7051115db94ed40e9abb2c0766e80e228ecad60e06b3b397acab" "d5a878172795c45441efcd84b20a14f553e7e96366a163f742b95d65a3f55d71" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "16ce45f31cdea5e74ca4d27519d7ebe998d69ec3bf7df7be63c5ffdb5638b387" default))
 '(linum-format " %7i ")
 '(package-selected-packages
   '(busybee-theme cmake-mode simpleclip sqlformat csv-mode consult-selectrum consult poetry ob-graphql rainbow-delimiters ag yasnippet yaml-mode window-numbering which-key web-mode visual-regexp use-package undo-tree typescript-mode twilight-theme tommyh-theme terraform-doc tangotango-theme tango-plus-theme sublime-themes smartparens selectrum-prescient rainbow-mode py-isort paper-theme ob-http nginx-mode mustang-theme magit lsp-pyright lsp-haskell inf-clojure highlight-numbers hideshow-org greymatters-theme golden-ratio github-theme feature-mode expand-region exec-path-from-shell eshell-z eshell-up eshell-toggle eshell-git-prompt emmet-mode doom-themes dockerfile-mode docker delight ctrlf crux company-terraform company-prescient comment-dwim-2 cloud-theme badwolf-theme))
 '(safe-local-variable-values
   '((python-shell-interpreter . "poetry run outsight/manage.py shell")
     (eval add-hook 'after-save-hook
           (lambda nil
             (org-babel-tangle)
             (byte-recompile-file "~/.config/emacs/init.el"))
           nil t)))
 '(show-paren-mode t)
 '(warning-suppress-types '((comp))))
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
