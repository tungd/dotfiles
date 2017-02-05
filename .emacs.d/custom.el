(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#282c34" "#ff6c6b" "#98be65" "#da8548" "#61afef" "#c678dd" "#1f5582" "#abb2bf"])
 '(ansi-term-color-vector
   [unspecified "#1d2021" "#fb543f" "#95c085" "#fac03b" "#0d6678" "#8f4673" "#0d6678" "#a89984"] t)
 '(custom-safe-themes
   (quote
    ("d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "47aa6e82734866b2915781c6e1d9517bd897d45fe8aec360dd4b6294fec73068" "dd4628d6c2d1f84ad7908c859797b24cc6239dfe7d71b3363ccdd2b88963f336" "e5f0f3c53f268ae8fc72cd569f99023d86187a7061f16196c350584b227e868f" "80ceeb45ccb797fe510980900eda334c777f05ee3181cb7e19cd6bb6fc7fda7c" "9c79dde113d5718497be6636b7358ec3ef3dad98d6c166fe88a8cdcd8b8abfc2" default)))
 '(fci-rule-color "#343d46")
 '(package-selected-packages
   (quote
    (exec-path-from-shell alchemist elixir-mode golden-ratio undo-tree fish-mode php-mode go-eldoc company-go go-mode docker-tramp rainbow-mode yasnippet yaml-mode wgrep-ag web-mode use-package tango-plus-theme sublime-themes smex smartparens smart-mode-line rainbow-delimiters pyvenv purescript-mode psc-ide projectile ob-http nlinum nix-mode nginx-mode markdown-mode magit intero indent-guide htmlize expand-region emmet-mode dumb-jump dockerfile-mode crux counsel comment-dwim-2 color-theme-sanityinc-tomorrow better-defaults base16-theme anzu ag)))
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(safe-local-variable-values
   (quote
    ((eval add-hook
           (quote after-save-hook)
           (lambda nil
             (org-babel-tangle))
           nil t))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#bf616a")
     (40 . "#DCA432")
     (60 . "#ebcb8b")
     (80 . "#B4EB89")
     (100 . "#89EBCA")
     (120 . "#89AAEB")
     (140 . "#C189EB")
     (160 . "#bf616a")
     (180 . "#DCA432")
     (200 . "#ebcb8b")
     (220 . "#B4EB89")
     (240 . "#89EBCA")
     (260 . "#89AAEB")
     (280 . "#C189EB")
     (300 . "#bf616a")
     (320 . "#DCA432")
     (340 . "#ebcb8b")
     (360 . "#B4EB89"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-delimiter-face ((t (:slant normal))))
 '(font-lock-comment-face ((t (:slant normal))))
 '(font-lock-constant-face ((t (:slant normal))))
 '(font-lock-keyword-face ((t (:weight normal))))
 '(font-lock-string-face ((t (:slant normal))))
 '(fringe ((t (:inherit font-lock-comment-face))))
 '(highlight ((t (:inherit region))))
 '(link ((t (:inherit font-lock-function-name-face :underline t))))
 '(linum ((t (:inherit font-lock-comment-face :height 110))))
 '(minibuffer-prompt ((t (:inherit font-lock-function-name-face))))
 '(rainbow-delimiters-unmatched-face ((t (:inherit error :background "#f00"))))
 '(sp-pair-overlay-face ((t (:inherit region))))
 '(vertical-border ((t (:foreground "#000" :background "#000")))))
