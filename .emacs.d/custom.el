(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("840db7f67ce92c39deb38f38fbc5a990b8f89b0f47b77b96d98e4bf400ee590a" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "cbd8e65d2452dfaed789f79c92d230aa8bdf413601b261dbb1291fb88605110c" "12670281275ea7c1b42d0a548a584e23b9c4e1d2dabb747fd5e2d692bcd0d39b" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "5a7830712d709a4fc128a7998b7fa963f37e960fd2e8aa75c76f692b36e6cf3c" "ffe80c88e3129b2cddadaaf78263a7f896d833a77c96349052ad5b7753c0c5a5" "36746ad57649893434c443567cb3831828df33232a7790d232df6f5908263692" "daa7ac1dde9d089a998fa2f90c19354fc4ef12bcfd312aca1bcf003a7c381501" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "3de3f36a398d2c8a4796360bfce1fa515292e9f76b655bb9a377289a6a80a132" "d96587ec2c7bf278269b8ec2b800c7d9af9e22d816827639b332b0e613314dfd" "aeb698d431751b18153e89b5f838fc3992433780a39a082740db216c7202a1c9" "cabc32838ccceea97404f6fcb7ce791c6e38491fd19baa0fcfb336dcc5f6e23c" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "3f546f127564de59f1c42dfd65d32bd7650fa5a614cd58c2df061da052ea3ec0" "73ad471d5ae9355a7fa28675014ae45a0589c14492f52c32a4e9b393fcc333fd" "41eb3fe4c6b80c7ad156a8c52e9dd6093e8856c7bbf2b92cc3a4108ceb385087" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "1263771faf6967879c3ab8b577c6c31020222ac6d3bac31f331a74275385a452" "04790c9929eacf32d508b84d34e80ad2ee233f13f17767190531b8b350b9ef22" "e1498b2416922aa561076edc5c9b0ad7b34d8ff849f335c13364c8f4276904f0" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "a62f0662e6aa7b05d0b4493a8e245ab31492765561b08192df61c9d1c7e1ddee" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(package-selected-packages
   (quote
    (dot-mode visual-regexp-steroids visual-regexp rainbow-delimiters htmlize hydra nginx-mode yasnippet yaml-mode wgrep-ag web-mode use-package undo-tree sublime-themes smex smartparens smart-mode-line realgud rainbow-mode pyvenv purescript-mode psc-ide projectile ob-http nlinum nix-mode magit kite-mini intero indent-guide fish-mode exec-path-from-shell emmet-mode editorconfig dumb-jump dockerfile-mode crux counsel comment-dwim-2 better-defaults base16-theme anzu alchemist ag)))
 '(safe-local-variable-values
   (quote
    ((eval add-hook
           (quote after-save-hook)
           (lambda nil
             (org-babel-tangle))
           nil t)
     (eval add-hook
           (quote after-save-hook)
           (function td/after-save-tangle-init)
           nil t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(highlight ((t (:inherit region))))
 '(link ((t (:inherit font-lock-function-name-face :underline t))))
 '(linum ((t (:inherit font-lock-comment-face :height 110))))
 '(minibuffer-prompt ((t (:inherit font-lock-function-name-face))))
 '(rainbow-delimiters-unmatched-face ((t (:inherit error))))
 '(sp-pair-overlay-face ((t (:inherit (quote region)))))
 '(vertical-border ((t (:foreground "#000" :background "#000")))))
