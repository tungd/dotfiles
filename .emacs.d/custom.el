(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#fbf1c7" "#9d0006" "#79740e" "#b57614" "#076678" "#b16286" "#427b58" "#282828"])
 '(custom-safe-themes
   '("93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "632694fd8a835e85bcc8b7bb5c1df1a0164689bc6009864faed38a9142b97057" "9b272154fb77a926f52f2756ed5872877ad8d73d018a426d44c6083d1ed972b1" "e2acbf379aa541e07373395b977a99c878c30f20c3761aac23e9223345526bcc" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "071f5702a5445970105be9456a48423a87b8b9cfa4b1f76d15699b29123fb7d8" default))
 '(datetime-timezone 'Asia/Ho_Chi_Minh)
 '(fci-rule-color "#504945")
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#a89984"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#79740e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#928374"))
 '(objed-cursor-color "#9d0006")
 '(package-selected-packages
   '(vterm mini-modeline doom-modeline delight company-box highlight-indentation typescript-mode feature-mode docker haskell-mode dune utop eldoc-box eldoc-overlay expand-region ivy-posframe use-package anzu tuareg yasnippet yaml-mode window-numbering web-mode undo-tree tango-plus-theme swift-mode solidity-mode smex smartparens smart-mode-line rust-mode request rainbow-mode rainbow-delimiters pyvenv py-isort ob-sql-mode ob-http nginx-mode markdown-mode magit kotlin-mode ivy-historian indent-guide highlight-numbers groovy-mode go-mode exec-path-from-shell emmet-mode eglot dumb-jump doom-themes dockerfile-mode diminish diff-hl crux counsel company-statistics comment-dwim-2 ag add-node-modules-path 0blayout))
 '(pdf-view-midnight-colors (cons "#282828" "#fbf1c7"))
 '(rustic-ansi-faces
   ["#fbf1c7" "#9d0006" "#79740e" "#b57614" "#076678" "#b16286" "#427b58" "#282828"])
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
           (lambda nil
             (org-babel-tangle)
             (byte-recompile-file "~/.config/emacs/init.el"))
           nil t)
     (eval add-hook 'after-save-hook
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
           nil t)))
 '(vc-annotate-background "#fbf1c7")
 '(vc-annotate-color-map
   (list
    (cons 20 "#79740e")
    (cons 40 "#8d7410")
    (cons 60 "#a17512")
    (cons 80 "#b57614")
    (cons 100 "#b3620e")
    (cons 120 "#b14e08")
    (cons 140 "#af3a03")
    (cons 160 "#af472e")
    (cons 180 "#b0545a")
    (cons 200 "#b16286")
    (cons 220 "#aa415b")
    (cons 240 "#a32030")
    (cons 260 "#9d0006")
    (cons 280 "#9a2021")
    (cons 300 "#97413c")
    (cons 320 "#946258")
    (cons 340 "#504945")
    (cons 360 "#504945")))
 '(vc-annotate-very-old-color nil))

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
 '(line-number ((t :inherit font-lock-comment-face)))
 '(markdown-inline-code-face ((t (:slant normal))))
 '(nix-attribute-face ((t (:inherit font-lock-builtin-face))))
 '(rainbow-delimiters-unmatched-face ((t (:inherit error :background "#f00"))))
 '(vertical-border ((t (:foreground "#000" :background "#000"))))
 '(web-mode-variable-name-face ((t (:inherit default)))))
