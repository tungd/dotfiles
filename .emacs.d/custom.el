(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(ansi-term-color-vector
   [unspecified "#2d2d2d" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#6699cc" "#d3d0c8"])
 '(beacon-color "#ed0547ad8099")
 '(custom-safe-themes
   '("e7666261f46e2f4f42fd1f9aa1875bdb81d17cc7a121533cad3e0d724f12faf2" "2878517f049b28342d7a360fd3f4b227086c4be8f8409f32e0f234d129cee925" "2d1fe7c9007a5b76cea4395b0fc664d0c1cfd34bb4f1860300347cdad67fb2f9" "ab25aefc2520a27724993adf04aae670020aaa56e820b5950db048c23f4dc703" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "fefab1b6d3366a959c78b4ed154018d48f4ec439ce652f4748ef22945ca7c2d5" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "0fe9f7a04e7a00ad99ecacc875c8ccb4153204e29d3e57e9669691e6ed8340ce" "423435c7b0e6c0942f16519fa9e17793da940184a50201a4d932eafe4c94c92d" "8be07a2c1b3a7300860c7a65c0ad148be6d127671be04d3d2120f1ac541ac103" default))
 '(datetime-timezone 'Asia/Ho_Chi_Minh)
 '(evil-emacs-state-cursor '("#E57373" hbar))
 '(evil-insert-state-cursor '("#E57373" bar))
 '(evil-normal-state-cursor '("#FFEE58" box))
 '(evil-visual-state-cursor '("#C5E1A5" box))
 '(fci-rule-color "#5c5e5e")
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   '("#FFEE58" "#C5E1A5" "#80DEEA" "#64B5F6" "#E1BEE7" "#FFCC80"))
 '(highlight-symbol-foreground-color "#E0E0E0")
 '(highlight-tail-colors '(("#ed0547ad8099" . 0) ("#424242" . 100)))
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(jdee-db-active-breakpoint-face-colors (cons "#0d0d0d" "#81a2be"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#0d0d0d" "#b5bd68"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#0d0d0d" "#5a5b5a"))
 '(objed-cursor-color "#cc6666")
 '(package-selected-packages
   '(use-package anzu inkpot-theme apropospriate-theme spacemacs-theme tuareg nim-mode yasnippet yaml-mode window-numbering web-mode undo-tree tango-plus-theme swift-mode solidity-mode soft-morning-theme smex smartparens smart-mode-line rust-mode request rainbow-mode rainbow-delimiters pyvenv py-isort purescript-mode psc-ide ob-sql-mode ob-http nginx-mode markdown-mode magit kotlin-mode kaolin-themes ivy-historian indent-guide highlight-numbers haskell-mode groovy-mode go-mode expand-region exec-path-from-shell emmet-mode eglot dumb-jump doom-themes dockerfile-mode docker diminish diff-hl dhall-mode crux counsel company-statistics comment-dwim-2 benchmark-init aria2 ag add-node-modules-path 0blayout))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(pos-tip-background-color "#3a933a933a93")
 '(pos-tip-foreground-color "#9E9E9E")
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
           (lambda nil
             (org-babel-tangle)
             (async-byte-compile-file "~/.emacs.d/init.el"))
           nil t)
     (eval add-hook 'after-save-hook
           (lambda nil
             (org-babel-tangle)
             (byte-recompile-file "~/.emacs.d/init.el"))
           nil t)))
 '(tabbar-background-color "#357535753575")
 '(vc-annotate-background "#1d1f21")
 '(vc-annotate-color-map
   (list
    (cons 20 "#b5bd68")
    (cons 40 "#c8c06c")
    (cons 60 "#dcc370")
    (cons 80 "#f0c674")
    (cons 100 "#eab56d")
    (cons 120 "#e3a366")
    (cons 140 "#de935f")
    (cons 160 "#d79e84")
    (cons 180 "#d0a9a9")
    (cons 200 "#c9b4cf")
    (cons 220 "#ca9aac")
    (cons 240 "#cb8089")
    (cons 260 "#cc6666")
    (cons 280 "#af6363")
    (cons 300 "#936060")
    (cons 320 "#765d5d")
    (cons 340 "#5c5e5e")
    (cons 360 "#5c5e5e")))
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
 '(line-number ((t (:height 120))))
 '(line-number-current-line ((t (:height 120 :bold nil))))
 '(nix-attribute-face ((t (:inherit font-lock-builtin-face))))
 '(rainbow-delimiters-unmatched-face ((t (:inherit error :background "#f00"))))
 '(vertical-border ((t (:foreground "#000" :background "#000"))))
 '(web-mode-variable-name-face ((t (:inherit default)))))
