;;; init.el -- Emacs initialization file
;;
;;; Commentary:
;; Sections:
;; - Package loading
;; - Global variable declaration
;; - Editing
;; - Navigation
;; - Programming language support
;; - UI
;; - Utilities
;;
;;; Code:

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(add-to-list 'load-path (concat user-emacs-directory "vendor/"))


;;;; Globals

;; I rarely turn off the computer, so it's ok to have these in /tmp.
;; You know, auto cleanup as a service ;)
(defvar td/data-directory "/tmp/")
(setq backup-directory-alist `((".*" . ,td/data-directory))
      auto-save-list-file-prefix td/data-directory
      auto-save-timeout (* 5 60)
      auto-save-file-name-transforms `((".*" ,td/data-directory))
      create-lockfiles nil
      ring-bell-function 'ignore)

(setq user-full-name "Tung Dao"
      user-mail-address "me@tungdao.com"
      default-input-method 'vietnamese-telex)

(setq-default
 visible-bell nil
 inhibit-startup-screen t
 delete-by-moving-to-trash t
 custom-file (expand-file-name "custom.el" user-emacs-directory))

(defalias 'yes-or-no-p 'y-or-n-p)

(load custom-file :no-error)

(savehist-mode t)
(save-place-mode t)

(use-package server
  :commands (server-running-p)
  :init
  (progn
    (defun td/start-server ()
      (interactive)
      (unless (server-running-p)
        (server-start)))

    (add-hook 'after-init-hook #'td/start-server)))

(when (eq system-type 'darwin)
  (setq trash-directory "~/.Trash/"
        mac-option-modifier 'super
        mac-command-modifier 'meta)

  (if (fboundp 'mac-auto-operator-composition-mode)
      (mac-auto-operator-composition-mode)))

(use-package exec-path-from-shell
  :ensure t
  :defer t
  :init
  (progn
    (setq-default exec-path-from-shell-check-startup-files nil)
    (exec-path-from-shell-initialize))
  :config
  (progn
    (exec-path-from-shell-copy-envs '("PYENV_ROOT"))
    (setenv "WORKON_HOME" (expand-file-name "versions" (getenv "PYENV_ROOT")))))

;;;; Editing
(setq-default
 indent-tabs-mode nil
 tab-width 2
 require-final-newline t
 echo-keystrokes 0.1
 scroll-margin 4)

(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(global-auto-revert-mode t)
(pending-delete-mode t)

(setq-default
 comment-auto-fill-only-comments t
 fill-column 80)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)

(use-package misc
  :commands (zap-up-to-char)
  :bind (("M-z" . zap-up-to-char)))

(defun td/ends-with-colon ()
  "TODO: docs."
  (interactive)
  (end-of-line)
  (insert ":"))

(defun td/ends-with-semicolon ()
  "TODO: docs."
  (interactive)
  (end-of-line)
  (insert ";"))

(bind-keys ("C-:" . td/ends-with-colon)
           ("C-;" . td/ends-with-semicolon))

(bind-key [remap delete-horizontal-space] #'cycle-spacing)

(use-package editorconfig
  :defer t
  :ensure t
  :init (editorconfig-mode t))

(use-package whitespace
  :commands (whitespace-cleanup
             whitespace-mode)
  :bind ("C-c w" . whitespace-mode)
  :init
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (progn
    (add-to-list 'whitespace-display-mappings
                 '(newline-mark ?\n [?\u00AC ?\n] [?$ ?\n]) t)

    (setq whitespace-line-column nil
          whitespace-style
          '(face
            tabs tab-mark
            spaces space-mark
            newline newline-mark
            trailing lines-tail
            space-before-tab space-after-tab))

    (add-hook 'before-save-hook #'whitespace-cleanup)
    (add-hook 'before-save-hook #'delete-trailing-whitespace)))

(use-package yasnippet
  :commands yas-global-mode
  :init
  (progn
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    (yas-global-mode t))
  :config
  (progn
    (setq yas-prompt-functions
          '(yas-ido-prompt yas-completing-prompt yas-no-prompt)
          ;; Suppress excessive log messages
          yas-verbosity 1
          ;; I am a weird user, I use SPACE to expand my
          ;; snippets, this save me from triggering them accidentally.
          yas-expand-only-for-last-commands
          '(self-insert-command org-self-insert-command))

    (unbind-key "TAB" yas-minor-mode-map)
    (unbind-key "<tab>" yas-minor-mode-map)
    (bind-key "SPC" 'yas-expand yas-minor-mode-map)))

(use-package expand-region
  :ensure t
  :defer t
  :bind ("M--" . er/expand-region))

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("M-C-9" . mc/mark-previous-like-this)
         ("M-C-0" . mc/mark-next-like-this)
         ("M-(" . mc/skip-to-previous-like-this)
         ("M-)" . mc/skip-to-next-like-this)
         ("C-c C-a" . mc/mark-all-like-this)
         ("C-x SPC" . set-rectangular-region-anchor)))

(use-package company
  :ensure t
  :defer t
  :bind ("M-/" . company-complete-common-or-cycle)
  :init (global-company-mode t)
  :config
  (progn
    (use-package company-statistics
      :ensure t
      :defer t
      :init (company-statistics-mode t))

    (setq company-minimum-prefix-length 2
          company-require-match nil
          company-idle-delay nil
          company-tooltip-align-annotations t
          company-frontends
          '(company-pseudo-tooltip-unless-just-one-frontend
            company-echo-metadata-frontend)
          company-backends
          '((company-dabbrev-code
             :with
             company-capf
             company-yasnippet)))

    (bind-keys :map company-active-map
               ("<tab>" . company-complete-common-or-cycle)
               ("C-n" . company-select-next-or-abort)
               ("C-p" . company-select-previous-or-abort))))

(use-package comment-dwim-2
  :ensure t
  :bind ([remap comment-dwim] . comment-dwim-2)
  :config (setq comment-style 'multi-line))

(use-package flyspell
  :defer t
  :bind (("C-c s" . ispell-word))
  :config
  (setq flyspell-prog-text-faces
        '(font-lock-comment-face font-lock-doc-face font-lock-doc-string-face))
  :init
  (progn
    ;; (add-hook 'prog-mode-hook #'flyspell-prog-mode)
    (add-hook 'text-mode-hook #'flyspell-mode)))

(use-package align
  :defer t
  :bind ("C-c =" . align)
  :config
  (progn
    (add-to-list 'align-rules-list
                 '(js-object-props
                   (regexp . "\\(\\s-*\\):")
                   (modes . '(js-mode js2-mode))
                   (spacing . 0)))))

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode t)
  :config
  (setq undo-tree-mode-lighter ""
        undo-tree-visualizer-timestamps t
        ;; Not working with yasnippet, I'm supposed to fix this but...
        ;; undo-tree-auto-save-history t
        ;; undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undos"))
        ))

;;;; Navigation
(defun td/next-ten-visual-line ()
  "TODO: docs."
  (interactive)
  (next-logical-line 10))

(defun td/previous-ten-visual-line ()
  "TODO: docs."
  (interactive)
  (next-logical-line -10))

(bind-keys ("M-n" . td/next-ten-visual-line)
           ("M-p" . td/previous-ten-visual-line))

(defun td/kill-current-buffer ()
  "Kill current the buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(bind-key* "C-c C-k" #'td/kill-current-buffer)

(use-package recentf
  :defer t
  :init (recentf-mode t)
  :config
  (setq recentf-max-saved-items 64
        recentf-auto-cleanup 'never))

(setq-default
 search-default-mode t
 isearch-lazy-highlight-initial-delay 0)

(bind-keys :map isearch-mode-map
           ("<backspace>" . isearch-del-char))

(use-package thingatpt
  :defer t
  :init
  (progn
    (defun td/bounds-of-buffer-at-point ()
      (cons (point-min) (point-max)))

    (put 'buffer 'bounds-of-thing-at-point 'td/bounds-of-buffer-at-point)
    (put 'buffer 'beginning-op 'beginning-of-buffer)
    (put 'buffer 'end-op 'end-of-buffer)))

(use-package window-numbering
  :ensure t
  :defer t
  :init (window-numbering-mode t)
  :config
  (progn
    (defun td/bracket-window-number-string (fn &rest args)
      (format "[%s] " (apply fn args)))

    (advice-add 'window-numbering-get-number-string
                :around #'td/bracket-window-number-string)))

(use-package projectile
  :ensure t
  :defer t
  :init (projectile-global-mode t)
  :bind ("C-M-'" . projectile-find-file-dwim)
  :config
  (setq projectile-globally-ignored-file-suffixes
        '("jpg" "png" "svg" "psd" "sketch" "afdesign"
          "pdf" "doc" "docx" "xls" "xlsx"
          "ttf" "otf" "woff"
          "rar" "zip")))

(use-package dumb-jump
  :ensure t
  :init (dumb-jump-mode t))

(use-package swiper
  :defer t
  :ensure t
  :bind (("C-M-l" . swiper)))

(use-package ivy
  :defer t
  :init (ivy-mode t)
  :bind (("C-M-o" . ivy-switch-buffer))
  :config
  (progn
    (setq ivy-format-function 'ivy-format-function-arrow
          ivy-count-format ""
          ivy-use-virtual-buffers t
          ivy-height 16
          projectile-completion-system 'ivy)

    (use-package flx
      :ensure t
      :init
      (setq ivy-re-builders-alist '((t . ivy--regex-plus))
            ivy-initial-inputs-alist nil))

    (require 'ivy-popup)))

(use-package counsel
  :ensure t
  :defer t
  :bind (([remap find-file] . counsel-find-file)
         ([remap execute-extended-command] . counsel-M-x)
         ("M-m" . counsel-M-x)
         ("C-c i" . counsel-imenu)))

(use-package smex
  :ensure t
  :init (smex-initialize))

(use-package imenu
  :defer t
  :config
  (setq imenu-auto-rescan t))

(use-package ag
  :defer t
  :ensure t)

(use-package wgrep-ag
  :defer t
  :ensure t)

(use-package smartparens
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'smartparens-mode)
  :bind (("M-s DEL" . sp-splice-sexp)
         ("M-S" . sp-rewrap-sexp)
         ("M-s <right>" . sp-slurp-hybrid-sexp)
         ("M-s <left>" . sp-forward-barf-sexp)))


;;;; Programming langauges
;;;; Web
(use-package emmet-mode
  :ensure t
  :defer t
  :commands emmet-mode
  :init
  (progn
    (defun td/emmet-jsx-mode ()
      (interactive)
      (emmet-mode t)
      (setq-local emmet-expand-jsx-className? t))

    (add-hook 'sgml-mode-hook #'emmet-mode)
    (add-hook 'web-mode-hook #'emmet-mode)
    (add-hook 'css-mode-hook #'emmet-mode)
    (add-hook 'js2-jsx-mode-hook #'td/emmet-jsx-mode))
  :config
  (progn
    (setq emmet-indentation 2
          emmet-preview-default nil
          emmet-insert-flash-time 0.1)

    (defun td/hide-emmet-preview-tooltip ()
      (overlay-put emmet-preview-output 'before-string nil))

    (advice-add 'emmet-preview
                :after #'td/hide-emmet-preview-tooltip)))

(use-package sgml-mode
  :defer t
  :mode (("\\.xml" . sgml-mode)
         ("\\.xjb" . sgml-mode)))

(use-package web-mode
  :ensure t
  :defer t
  :mode (("\\.html" . web-mode)
         ("\\.j2" . web-mode)
         ("\\.jinja2" . web-mode))
  :init (add-hook 'web-mode-hook #'emmet-mode)
  :config
  (progn
    (add-hook 'web-mode-hook #'turn-off-auto-fill)

    (setq web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2)))

(defun td/css-imenu-expressions ()
  "TODO: docs."
  (set (make-local-variable 'imenu-generic-expression)
       '(("Section" "^.*\\* =\\(.+\\)$" 1)
         (nil "^\\(.+\\)\\S*{$" 1))))

(use-package css-mode
  :mode (("\\.css$" . css-mode))
  :config
  (progn
    (setq css-indent-offset 2)
    (add-hook 'css-mode-hook #'td/css-imenu-expressions)))

(use-package kite-mini
  :ensure t
  :defer t)

;;;; Javascript
(use-package js
  :mode (("\\.json$" . js-mode))
  :config
  (setq js-indent-level 2))

(use-package js2-mode
  :ensure t
  :mode (("\\.js$" . js2-mode)
         ("\\.jsx" . js2-jsx-mode))
  :config (setq js2-basic-offset 2
                js2-include-node-externs t
                js2-highlight-level 3
                js2-mode-show-parse-errors nil
                js2-strict-missing-semi-warning nil))

;;;; Clojure
(use-package clojure-mode
  :ensure t
  :defer t
  :mode (("\\.clj$" . clojure-mode)
         ("build\\.boot$" . clojure-mode)))

(use-package inf-clojure
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
    (add-hook 'clojure-mode-hook #'eldoc-mode)
    (add-hook 'inf-clojure-mode-hook #'eldoc-mode))
  :config
  (progn
    (defun td/setup-clojurescript ()
      (interactive)
      (setq-local inf-clojure-load-command "(load-file \"%s\")\n")
      (setq-local inf-clojure-var-doc-command "(cljs.repl/doc %s)\n")
      (setq-local inf-clojure-var-source-command "(cljs.repl/source %s)\n")
      (setq-local inf-clojure-arglist-command "'()\n")
      (setq-local inf-clojure-completion-command "'()\n")
      (setq-local inf-clojure-ns-vars-command "(cljs.repl/dir %s)\n")
      (setq-local inf-clojure-set-ns-command "(in-ns '%s)\n")
      (setq-local inf-clojure-apropos-command "(doseq [var (sort (cljs.repl/apropos \"%s\"))]
                                                 (println (str var)))\n")
      (setq-local inf-clojure-macroexpand-command "(cljs.core/macroexpand '%s)\n")
      (setq-local inf-clojure-macroexpand-1-command "(cljs.core/macroexpand-1 '%s)\n"))

    (add-hook 'clojure-script-mode #'td/setup-clojurescript)))

;;;; Elixir
(use-package elixir-mode
  :ensure t
  :defer t)

(use-package alchemist
  :ensure t
  :defer t
  :init (add-hook 'elixir-mode-hook #'alchemist-mode))

;;;; Python
(use-package pyvenv
  :ensure t
  :defer t
  :config
  (progn
    (require 'seq)

    (defun td/pyenv-activate-project ()
      (seq-doseq (dir '("env" "venv" "virtualenv"))
        (let ((path (expand-file-name dir (cdr (project-current)))))
          (when (file-exists-p path)
            (path pyvenv-activate)
            (message "%s activated. Virtualenv" path)))))

    (add-hook 'projectile-after-switch-project-hook #'td/pyenv-activate-project)))



(use-package python
  :defer t
  :config
  (progn
    (defun td/run-python-with-project-root ()
      (interactive)
      (let ((default-directory (projectile-project-root)))
        (call-interactively 'run-python)))

    (bind-keys :map python-mode-map
               ([remap run-python] . td/run-python-with-project-root))))

;; PureScript
(use-package purescript-mode
  :ensure t
  :defer t
  :config
  (progn
    (defun purescript-doc-current-info ())

    (use-package psc-ide
      :ensure t
      :defer t)

    (defun td/setup-purescript ()
      "TODO: docs."
      (interactive)
      (psc-ide-mode)
      (turn-on-purescript-indentation))

    (add-hook 'purescript-mode-hook #'td/setup-purescript)))

;;;; Emacs Lisp
(use-package elisp-mode
  :config
  (progn
    (defun td/imenu-elisp-sections ()
      (setq imenu-prev-index-position-function nil)
      (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

    (add-hook 'emacs-lisp-mode-hook 'td/imenu-elisp-sections)))

;;;; Misc
(use-package yaml-mode
  :ensure t
  :mode (("\\.yaml$" . yaml-mode)
         ("\\.yml$" . yaml-mode)
         ("\\.sls$" . yaml-mode)))

(use-package sh-script
  :defer t
  :config
  (setq sh-basic-offset 2))

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile$" . dockerfile-mode))

(use-package nix-mode
  :ensure t
  :mode ("\\.nix$" . nix-mode))


;;;; Utilities
(use-package multi-term
  :ensure t
  :bind (("C-c n o" . multi-term)
         ("C-c n n" . multi-term-next)
         ("C-c n m" . multi-term-prev))
  :config
  (setq multi-term-program "/bin/bash"))

(use-package comint
  :defer t
  :init
  (progn
    (add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)
    (add-hook 'comint-output-filter-functions #'comint-truncate-buffer)

    (defun td/clear-comint ()
      (interactive)
      (let ((comint-buffer-maximum-size 0))
        (comint-truncate-buffer)))

    (bind-keys :map comint-mode-map
               ("C-c C-s" . td/clear-comint))))

(use-package vc-hooks
  :defer t
  :config (setq vc-follow-symlinks t))

(use-package ztree
  :ensure t
  :defer t)

(use-package workgroups2
  :ensure t
  :defer t
  :config
  (progn
    (setq wg-mode-line-decor-left-brace "["
          wg-mode-line-decor-right-brace "]")

    (add-hook 'wg-before-switch-to-workgroup-hook #'wg-save-session)))

(use-package tramp
  :defer t
  :config
  (progn
    (setq password-cache-expiry nil
          tramp-default-method "ftp")

    (add-to-list 'auth-sources "~/.emacs.d/authinfo.gpg")))

(use-package ange-ftp
  :defer t
  :config
  (setq ange-ftp-netrc-filename "~/.emacs.d/authinfo.gpg"))

(use-package org
  :defer t
  :bind (("C-c o a" . org-agenda)
         ("C-c o t" . org-todo-list))
  :config
  (progn
    (setq org-directory "~/Dropbox (Personal)/GTD/"
          org-ellipsis "…"
          org-default-notes-file (expand-file-name "inbox.org" org-directory)
          org-log-done 'time
          org-todo-keywords
          '((sequence "TODO(t)" "STARTED(s!)" "WAITING(w@/!)"
                      "|" "CANCELED(c@)" "DONE(d!)"))
          org-src-fontify-natively t
          org-src-tab-acts-natively t
          org-hide-leading-stars t)

    (setq org-agenda-files `(,org-default-notes-file)
          org-agenda-skip-unavailable-files t)

    (defun td/org-ispell ()
      "Configure `ispell-skip-region-alist' for `org-mode'."
      (make-local-variable 'ispell-skip-region-alist)
      (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
      (add-to-list 'ispell-skip-region-alist '("~" "~"))
      (add-to-list 'ispell-skip-region-alist '("=" "="))
      (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC")))
    (add-hook 'org-mode-hook #'td/org-ispell)

    (use-package ob-http
      :ensure t
      :config
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((emacs-lisp . t)
         (http . t))))
    (setq org-confirm-babel-evaluate nil)))

(use-package org-agenda
  :config
  (setq org-agenda-skip-deadline-if-done nil
        org-agenda-skip-scheduled-if-done nil
        org-agenda-restore-windows-after-quit t
        org-agenda-window-setup 'current-window
        org-agenda-show-all-dates t
        org-agenda-show-log t))

(use-package magit
  :ensure t
  :defer t
  :config
  (progn
    (setq magit-display-buffer-function
          #'magit-display-buffer-fullframe-status-v1)))

(use-package dired
  :defer t
  :config
  (progn
    (dired-async-mode t)

    (setq dired-listing-switches "-alh"
          dired-recursive-copies 'always)

    (bind-keys :map dired-mode-map
               ("C-c '" . wdired-change-to-wdired-mode))))

(use-package compile
  :defer t
  :bind ("C-c m" . recompile)
  :config
  (setq compilation-scroll-output t))

(use-package ibuffer
  :defer t
  :bind ([remap list-buffers] . ibuffer))

(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode t)
  :config
  (progn
    (setq-default flycheck-disabled-checkers
                  '(scss))

    ;; Disable `js2-mode' built-in error checker
    (eval-after-load 'js2-mode
      '(setq js2-mode-show-parse-errors nil))

    (setq flycheck-checkers
          (--remove (eq it 'emacs-lisp-checkdoc) flycheck-checkers))))

(use-package ediff
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

(use-package realgud
  :ensure t)

(use-package crux
  :ensure t
  :commands (crux-sudo-edit
             crux-switch-to-previous-buffer
             crux-top-join-line
             crux-cleanup-buffer-or-region
             crux-kill-whole-line)
  :bind (("C-M-]" . crux-switch-to-previous-buffer)
         ("M-J" . crux-top-join-line)
         ("M-=" . crux-cleanup-buffer-or-region)
         ("C-M-k" . crux-kill-whole-line)))

(use-package info
  :defer t
  :config
  (add-to-list 'Info-directory-list (expand-file-name "info/" user-emacs-directory)))

;;;; UI
(setq-default
 line-spacing 2
 fringes-outside-margins t
 indicate-empty-lines t
 default-frame-alist
 '((font . "Source Code Pro Medium 14")
   ;; (top . 0) (left . 0) (width . 180) (height . 64)
   (fullscreen . fullboth)
   (vertical-scroll-bars . nil)
   (menu-bar-lines . 0)
   (tool-bar-lines . 0)
   (right-fringe . 0)))

(blink-cursor-mode -1)
(electric-pair-mode t)
(column-number-mode t)

(use-package highlight-parentheses
  :ensure t
  :init (global-highlight-parentheses-mode t)
  :config
  (setq hl-paren-colors '("firebrick1")
        hl-paren-delay 0.01))

(use-package paren-face
  :ensure t
  :init (global-paren-face-mode t)
  :config
  (progn
    (setq paren-face-modes '(prog-mode))
    (set-face-attribute 'parenthesis nil :foreground "#666")))

(use-package base16-theme
  :ensure t
  :init (load-theme 'base16-bespin t))

(unless (display-graphic-p)
  (set-face-attribute 'default nil :background "black"))

(define-fringe-bitmap 'tilde
  [#b01110001
   #b11011011
   #b00001110]
  nil nil 'center)

(define-fringe-bitmap 'halftone
  [#b01000000
   #b10000000]
  nil nil '(top t))

(setcdr (assq 'continuation fringe-indicator-alist) 'halftone)
(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
(set-fringe-bitmap-face 'tilde 'font-lock-comment-face)
(set-face-attribute 'vertical-border nil :foreground "#000" :background "#000")
(set-display-table-slot standard-display-table
                        'vertical-border (make-glyph-code ?│))

(use-package smart-mode-line
  :ensure t
  :defer t
  :init (sml/setup)
  :config
  (progn
    (add-to-list 'sml/replacer-regexp-list
                 '("^~/Projects/dotfiles/\\(.*\\)/" ":Config:\\1:"))

    (use-package rich-minority
      :config
      (setq rm-blacklist
            (append rm-blacklist
                    '(" wg" " hs" " snipe" " ivy"
                      " Fill" " Undo-Tree" " yas" " company" " SP" " Anzu" " ARev"))))))

(use-package nlinum
  :defer t
  :ensure t
  :commands (nlinum-mode)
  :config
  (progn
    (setq-default nlinum-format " %4d ")
    ;; nlinum-highlight-current-line t

    (defun td/nlinum-custom-faces ()
      "Custom faces for `nlinum'"
      (interactive)
      (require 'linum)
      (set-face-attribute 'linum nil
                          :height 100
                          :background (face-background 'font-lock-comment-face)
                          :foreground (face-foreground 'font-lock-comment-face))
      (set-face-attribute 'fringe nil
                          :background (face-background 'font-lock-comment-face)))

    (add-hook 'nlinum-mode-hook  #'td/nlinum-custom-faces))
  :init
  (progn
    (defun td/nlinum-may-turn-on ()
      "Turn on `nlinum' only if we're in GUI."
      (interactive)
      (when (display-graphic-p) (nlinum-mode t)))

    (add-hook 'prog-mode-hook #'td/nlinum-may-turn-on)))

(use-package diff-hl
  :ensure t
  :defer t
  :init (global-diff-hl-mode t)
  :config
  (progn
    (define-fringe-bitmap 'td/diff-hl-bmp-default
      [#b11000000] 1 8 '(top t))

    (define-fringe-bitmap 'td/diff-hl-bmp-delete
      [#b11111100
       #b11111000
       #b11110000
       #b11100000
       #b11000000
       #b10000000] nil 8 'top)

    (defun td/diff-hl-fringe-bmp (type pos)
      (cl-case type
        (delete 'td/diff-hl-bmp-delete)
        (t 'td/diff-hl-bmp-default)))

    (setq diff-hl-draw-borders t
          diff-hl-fringe-bmp-function #'td/diff-hl-fringe-bmp)

    (defun td/diff-hl-custom-faces ()
      (interactive)
      (set-face-attribute 'diff-hl-delete nil :background nil :foreground "#ff0000")
      (set-face-attribute 'diff-hl-change nil :background nil :foreground "#deae3e")
      (set-face-attribute 'diff-hl-insert nil :background nil :foreground "#81af34"))

    (advice-add 'diff-hl-overlay-modified :override #'ignore)

    (add-hook 'diff-hl-mode-hook #'td/diff-hl-custom-faces)
    (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(use-package hl-todo
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'hl-todo-mode))

(use-package ansi-color
  :defer t
  :init
  (progn
    (defun td/ansi-colorize-compilation-buffer ()
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region compilation-filter-start (point))))

    (add-hook 'compilation-filter-hook #'td/ansi-colorize-compilation-buffer)))

(use-package hideshow
  :defer t
  :bind (:map hs-minor-mode-map
              ("C-c C-m" . hs-toggle-hiding)
              ("C-c C-S-m" . hs-hide-all))
  :init
  (add-hook 'prog-mode-hook #'hs-minor-mode)
  :config
  (progn
    (defface hs-face
      '((t (:inherit font-lock-comment-face :height 120)))
      "Face for hideshow marker."
      :group 'hideshow)

    (define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0])

    (defun td/hs-setup-overlay (ov)
      (when (eq 'code (overlay-get ov 'hs))
        (let* ((marker-string "*fringe-dummy*")
               (marker-length (length marker-string))
               (display-string " {...}"))
          (overlay-put ov 'help-echo "Hiddent text. C-c C-m to show")
          (put-text-property 0 marker-length
                             'display
                             '(left-fringe hs-marker hs-face) marker-string)
          (overlay-put ov 'before-string marker-string)
          (put-text-property 1 (length display-string)
                             'face 'hs-face display-string)
          (overlay-put ov 'display display-string))))

    (setq hs-set-up-overlay 'td/hs-setup-overlay)))

(use-package indent-guide
  :ensure t
  :commands (indent-guide-mode)
  :init
  (progn
    (add-hook 'haml-mode-hook #'indent-guide-mode)
    (add-hook 'python-mode-hook #'indent-guide-mode)))

(use-package anzu
  :ensure t
  ;; Anzu command names are confusing, at-cursor means initial string, while
  ;; thing means boundary.
  :bind (([remap query-replace] . anzu-query-replace)
         ("C-c C-r" . anzu-query-replace-at-cursor)
         ("M-r" . anzu-replace-at-cursor-thing)
         ("C-M-r" . td/anzu-replace-at-cursor-thing-in-buffer))
  :init (add-hook 'after-init-hook #'global-anzu-mode)
  :config
  (progn
    (defun td/anzu-replace-at-cursor-thing-in-buffer ()
      "This does not actually query, but it's OK for me."
      (interactive)
      (let ((anzu-replace-at-cursor-thing 'buffer))
        (call-interactively 'anzu-query-replace-at-cursor-thing)))))

;; (use-package key-chord
;;   :ensure t
;;   :init
;;   (key-chord-mode t)
;;   :config
;;   (progn
;;     (defvar td/key-chords
;;       '(("<<" smart-shift-left)
;;         (">>" smart-shift-right)
;;         ("bb" switch-to-other-buffer)))

;;     (dolist (def td/key-chords)
;;       (key-chord-define-global (car def) (cadr def)))))

(use-package rainbow-mode
  :ensure t
  :defer t
  :init (add-hook 'css-mode-hook #'rainbow-mode))

(workgroups-mode t)

(provide 'init)
;;; init.el ends here
