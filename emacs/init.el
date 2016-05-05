;;; init.el -- Emacs initialization file
;;;
;;; Commentary:
;;;
;;; Code:

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(require 'use-package)

;;;;
(defvar td/data-directory "/tmp/")
(add-to-list 'load-path
             (concat (file-name-directory (or load-file-name (buffer-file-name))) "vendor/"))

;; I rarely turn off the computer, so it's ok to have these at /tmp.
;; You know, auto cleanup as a service ;)
(setq backup-directory-alist `((".*" . ,td/data-directory))
      auto-save-list-file-prefix td/data-directory
      auto-save-timeout (* 5 60)
      create-lockfiles nil
      ring-bell-function 'ignore)

;;
(setq user-full-name "Tung Dao"
      user-mail-address "me@tungdao.com"
      default-input-method 'vietnamese-telex)

(setq default-frame-alist
      '(;;(left-fringe . 0)
        (right-fringe . 4)
        (font . "Source Code Pro 12")
        (left . 256)
        (width . 180) (height . 55)
        (border-width . 0)
        (internal-border-width . 0)))

;; This is for emacsforosx.com version
(setq mac-option-modifier 'super
      mac-command-modifier 'meta)

(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(setq-default indent-tabs-mode nil
              tab-width 2)
(setq require-final-newline t
      echo-keystrokes 0.1)

(use-package etags
  :defer t
  :config
  (setq tags-revert-without-query t))

(use-package whitespace
  :commands (whitespace-cleanup
             whitespace-mode)
  :bind ("C-c w" . whitespace-mode)
  :init (add-hook 'before-save-hook #'whitespace-cleanup)
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

(setq-default comment-auto-fill-only-comments t
              fill-column 80)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)

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

;;
(setq visible-bell nil
      inhibit-startup-screen t
      delete-by-moving-to-trash t
      custom-file (expand-file-name "custom.el" user-emacs-directory))

(use-package exec-path-from-shell
  :ensure t
  :defer t)

(when (eq system-type 'darwin)
  (setq trash-directory "~/.Trash/")

  ;; BSD ls does not support --dired. Use GNU core-utils: brew install coreutils
  (when (executable-find "gls")
    (setq insert-directory-program "gls"))

  ;; Derive PATH by running a shell so that GUI Emacs sessions have access to it
  (exec-path-from-shell-initialize))

;; Never require full word answers
(defalias 'yes-or-no-p 'y-or-n-p)

(load custom-file :no-error)

(use-package color-theme-approximate
  :ensure t
  :init (color-theme-approximate-on))

(use-package solarized-theme
  :init
  (progn
    (defun td/setup-window ()
      (interactive)
      ;; Reload the current custom-theme to allow approximation
      (load-theme 'solarized t)
      ;; (set-face-attribute 'linum-highlight-face nil
      ;;                     :inherit 'linum
      ;;                     :background (or (face-foreground 'linum) "#ccc")
      ;;                     :foreground (or (face-background 'linum) "#fff"))
      (set-face-attribute 'fringe nil :background nil))

    (add-hook 'window-setup-hook #'td/setup-window)))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(electric-pair-mode t)
(column-number-mode t)
(unless (display-graphic-p)
  (menu-bar-mode -1))

(pending-delete-mode t)

(use-package recentf
  :defer t
  :init (recentf-mode t)
  :config
  (setq recentf-max-saved-items 64
        recentf-auto-cleanup 'never))

(use-package isearch
  :bind (([remap isearch-forward] . isearch-forward-regexp)
         ([remap isearch-backward] . isearch-backward-regexp))
  :config (setq lazy-highlight-initial-delay 0))

(use-package nlinum
  :defer t
  :ensure t
  :init (global-nlinum-mode t)
  :config
  (progn
    (setq nlinum-format " %4d ")
    (set-face-attribute 'linum nil :height 100)))

(use-package exec-path-from-shell
  :ensure t
  :defer t
  :init (exec-path-from-shell-initialize))

(use-package company
  :ensure t
  :defer t
  :bind ("M-/" . company-complete-common-or-cycle)
  :init (global-company-mode t)
  :config
  (progn
    (setq company-idle-delay 0.1
          company-minimum-prefix-length 4
          company-tooltip-align-annotations t
          company-tooltip-flip-when-above t
          company-tooltip-limit 16
          company-backends
          '(company-css
            company-capf
            (company-dabbrev-code company-gtags company-etags company-keywords)
            company-dabbrev
            company-files))

    (bind-keys :map company-active-map
               ("C-n" . company-select-next-or-abort)
               ("C-p" . company-select-previous-or-abort))))

(use-package company-go
  :ensure t
  :defer t
  :init (add-to-list 'company-backends 'company-go))

(use-package company-web
  :ensure t
  :defer t
  :init (add-to-list 'company-backends 'company-web-html))

(use-package company-statistics
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'company-statistics-mode))

(use-package savehist
  :defer t
  :init (savehist-mode t))

(use-package saveplace
  :init
  (progn
    (setq-default save-place t)))

(use-package thingatpt
  :init
  (progn
    (defun td/bounds-of-buffer-at-point ()
      (cons (point-min) (point-max)))

    (put 'buffer 'bounds-of-thing-at-point 'td/bounds-of-buffer-at-point)
    (put 'buffer 'beginning-op 'beginning-of-buffer)
    (put 'buffer 'end-op 'end-of-buffer)))

(use-package anzu
  :ensure t
  :defer t
  :init (global-anzu-mode t)
  :bind (([remap query-replace] . anzu-query-replace-regexp)
         ([remap query-replace-regexp] . td/anzu-smart-query-replace-regexp))
  :config
  (progn
    (defun td/anzu-smart-query-replace-regexp ()
      "This does not actually query, but it's OK for me."
      (interactive)
      (let ((anzu-replace-at-cursor-thing 'buffer))
        (call-interactively 'anzu-query-replace-at-cursor-thing)))))

(use-package osx-dictionary
  :ensure t
  :defer t
  :bind (("C-c d" . osx-dictionary-search-pointer)))

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

(use-package ace-jump-mode
  :ensure t
  :defer t
  :bind ("C-c C-SPC" . ace-jump-mode))

(use-package projectile
  :ensure t
  :defer t
  :init (projectile-global-mode t)
  :config
  (setq projectile-completion-system 'ivy))

(use-package diff-hl
  :ensure t
  :defer t
  :init (global-diff-hl-mode t)
  :config
  (progn
    (setq diff-hl-side 'right
          diff-hl-draw-borders nil)

    (defun diff-hl-overlay-modified (ov after-p beg end &optional len)
      "Markers disappear and reapear is kind of annoying to me.")))

(use-package magit
  :ensure t
  :defer t)

(use-package popwin
  :defer 1
  :ensure t
  :commands popwin-mode
  :init (popwin-mode t)
  :config
  (progn
    (bind-key "C-x p" popwin:keymap)

    (mapc (lambda (c)
            (add-to-list 'popwin:special-display-config c))
          '((occur-mode :noselect nil)
            ("*Org Agenda*" :width 60 :position right :dedicated t :stick t)
            ("*Compile-Log*" :height 20 :noselect t)
            ("*Ido Completions*" :noselect t :height 15)
            ("*cider-error*" :height 15 :stick t)
            ("*cider-doc*" :height 15 :stick t)
            ("*cider-src*" :height 15 :stick t)
            ("*cider-result*" :height 15 :stick t)
            ("*cider-macroexpansion*" :height 15 :stick t)
            (shell-mode :height 15)
            (ag-mode :height 15)))))

(use-package smart-mode-line
  :ensure t
  :defer t
  :init (sml/setup)
  :config
  (progn
    (add-to-list 'sml/replacer-regexp-list
                 '("^~/Projects/dotfiles/\\(.*\\)/" ":Config:\\1:"))

    (use-package rich-minority
      :init
      (progn
        (add-to-list 'rm-blacklist " Undo-Tree")
        (add-to-list 'rm-blacklist " Anzu")
        (add-to-list 'rm-blacklist " yas")
        (add-to-list 'rm-blacklist " company")))))

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
         ("M-C-a" . mc/mark-all-like-this)
         ("C-x SPC" . set-rectangular-region-anchor)))

(use-package comment-dwim-2
  :ensure t
  :bind ([remap comment-dwim] . comment-dwim-2)
  :config (setq comment-style 'multi-line))

(use-package paren
  :defer 1
  :init (setq-default show-paren-delay 0)
  :config (show-paren-mode t))

(add-hook 'after-init-hook #'server-start)

;;;;
(use-package org
  :defer t
  :config
  (setq org-directory "~/OneDrive/Documents/Notes/"
        org-default-notes-file (expand-file-name "inbox.org" org-directory)
        org-log-done 'time
        org-todo-keywords
        '((sequence "TODO(t)" "STARTED(s!)" "WAITING(w@/!)"
                    "|" "CANCELED(c@)" "DONE(d!)"))
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-hide-leading-stars t))

(use-package org-agenda
  :bind (("C-c o a" . org-agenda)
         ("C-c o t" . org-todo-list))
  :config
  (setq org-agenda-files `(,org-default-notes-file)
        org-agenda-skip-unavailable-files t
        org-agenda-skip-deadline-if-done nil
        org-agenda-skip-scheduled-if-done nil
        org-agenda-restore-windows-after-quit t
        org-agenda-window-setup 'current-window
        org-agenda-show-all-dates t
        org-agenda-show-log t))

(use-package ob-http
  :ensure t
  :defer t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (http . t))))

(use-package calc
  :defer t
  :bind ("C-c b c" . quick-calc))

(use-package flyspell
  :defer t
  :bind (("C-c s" . ispell-word))
  :init
  (progn
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)))

(use-package dired
  :defer t
  :config
  (progn
    (dired-async-mode t)

    (setq dired-listing-switches "-alh"
          dired-recursive-copies 'always)

    (bind-keys :map dired-mode-map
               ("C-c '" . wdired-change-to-wdired-mode))))

(use-package emmet-mode
  :ensure t
  :defer t
  :commands emmet-mode
  :init
  (progn
    (add-hook 'sgml-mode-hook #'emmet-mode)
    (add-hook 'web-mode-hook #'emmet-mode)
    (add-hook 'css-mode-hook #'emmet-mode))
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
                js2-strict-missing-semi-warning nil))

(use-package clojure-mode
  :ensure t
  :defer t
  :mode (("\\.clj$" . clojure-mode)
         ("\\.cljs$" . clojure-mode)
         ("build\\.boot$" . clojure-mode)))


(defun td/css-imenu-expressions ()
  "TODO: docs."
  (set (make-local-variable 'imenu-generic-expression)
       '(("Section" "^.*\\* =\\(.+\\)$" 1)
         (nil "^\\(.+\\)\\S*{$" 1))))

(use-package css-mode
  :defer t
  :mode (("\\.css$" . css-mode))
  :config
  (progn
    (setq css-indent-offset 2)
    (add-hook 'css-mode-hook #'td/css-imenu-expressions)))

(use-package scss-mode
  :ensure t
  :defer t
  :mode (("\\.scss$" . scss-mode)))

(use-package less-css-mode
  :ensure t
  :defer t
  :mode (("\\.less" . less-css-mode)))

(use-package yaml-mode
  :ensure t
  :defer t
  :mode (("\\.yaml$" . yaml-mode)
         ("\\.yml$" . yaml-mode)))

(use-package sh-script
  :defer t
  :config
  (setq sh-basic-offset 2))

(use-package go-mode
  :ensure t
  :defer t
  :mode (("\\.go$" . go-mode)))

(use-package php-mode
  :ensure t
  :defer t
  :mode (("\\.php$" . php-mode))
  :config
  (setq php-template-compatibility nil
        php-mode-coding-style 'drupal))

;;;;

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

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun td/join-next-line ()
  "TODO: docs."
  (interactive)
  (join-line -1))

(bind-key "M-J" #'td/join-next-line)

(defun td/cleanup-buffer ()
  "TODO: docs."
  (interactive)
  (save-excursion
    (whitespace-cleanup-region (point-min) (point-max))
    (indent-region (point-min) (point-max))))

(bind-key "M-=" #'td/cleanup-buffer)

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit. Will also prompt
for a file to visit if current buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(bind-key [remap delete-horizontal-space] #'just-one-space)

(bind-key "M-j" #'find-function-at-point)

(use-package which-func
  :defer t
  :init (which-function-mode t))

(use-package highlight-parentheses
  :ensure t
  :defer t
  :init (add-hook 'lisp-mode-hook #'highlight-parentheses-mode))

(use-package hl-todo
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'hl-todo-mode))

(bind-key "C-c m" #'recompile)

(use-package ansi-color
  :defer t
  :init
  (progn
    (defun td/ansi-colorize-compilation-buffer ()
      (read-only-mode t)
      (ansi-color-apply-on-region (point-min) (point-max))
      (read-only-mode -1))

    (add-hook 'compilation-filter-hook #'td/ansi-colorize-compilation-buffer)))

(use-package hideshow
  :defer t
  :bind (("C-c SPC" . hs-toggle-hiding)))

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode t)
  :config
  (setq undo-tree-mode-lighter ""
        undo-tree-visualizer-timestamps t
        undo-tree-auto-save-history t
        undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undos"))))

(use-package evil
  :ensure t
  :defer t
  :init (evil-mode t)
  :config
  (progn
    (setq evil-ex-substitute-global t
          evil-cross-lines t
          evil-move-cursor-back t)

    (bind-keys :map evil-insert-state-map
               ([remap newline] . newline-and-indent))

    (bind-keys :map evil-normal-state-map
               ("TAB" . evil-jump-item)
               ("<tab>" . evil-jump-item)
               ("j" . evil-next-visual-line)
               ("k" . evil-previous-visual-line)
               ("M-j" . td/evil-next-ten-visual-line)
               ("M-k" . td/evil-previous-ten-visual-line))

    (bind-keys :map evil-motion-state-map
               ("TAB" . evil-jump-item)
               ("<tab>" . evil-jump-item))

    (defun td/evil-next-ten-visual-line ()
      (interactive)
      (evil-next-visual-line 10))

    (defun td/evil-previous-ten-visual-line ()
      (interactive)
      (evil-previous-visual-line 10))

    (defun td/open-line ()
      (interactive)
      (end-of-line)
      (newline-and-indent))

    (defun td/ends-with-colon ()
      (interactive)
      (end-of-line)
      (insert ":"))

    (defun td/ends-with-semicolon ()
      (interactive)
      (end-of-line)
      (insert ";"))

    (bind-keys :map evil-insert-state-map
               ("C-e" . end-of-line)
               ((kbd "<C-return>") . td/open-line)
               ("C-;" . td/ends-with-semicolon)
               ("C-:" . td/ends-with-colon))))

(use-package evil-surround
  :ensure t
  :defer t
  :init (global-evil-surround-mode t))

(use-package evil-visualstar
  :ensure t
  :defer t
  :init (global-evil-visualstar-mode))

(use-package elixir-mode
  :ensure t
  :defer t)

(use-package alchemist
  :ensure t
  :defer t
  :init (add-hook 'elixir-mode-hook #'alchemist-mode))

(use-package eshell
  :defer t
  :config
  (progn
    (defmacro td/with-face (str &rest properties)
      `(propertize ,str 'face (list ,@properties)))

    (defun td/eshell-pwd ()
      (replace-regexp-in-string
       (regexp-quote (expand-file-name "~"))
       "~"
       (eshell/pwd)))

    (defun td/eshell-prompt ()
      (format
       "\n%s@%s in %s\n%s "
       (td/with-face user-login-name :foreground "#dc322f")
       (td/with-face (or (getenv "HOST") system-name) :foreground "#b58900")
       (td/with-face (td/eshell-pwd) :foreground "#859900")
       (if (= (user-uid) 0) (with-face "#" :foreground "red") "$")))

    (defalias 'eshell/e 'find-file-other-window)

    (defun eshell/open (args)
      (interactive)
      (shell-command
       (concat (case system-type
                 ((darwin) "open")
                 ((windows-nt) "start")
                 (t "xdg-open"))
               (format " %s" args))))

    (use-package em-prompt
      :defer t
      :config
      (setq eshell-prompt-function #'td/eshell-prompt
            eshell-prompt-regexp "^[^#$\\n]*[#$] "
            eshell-highlight-prompt nil))))

(add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)

(use-package vc-hooks
  :defer t
  :config (setq vc-follow-symlinks t))

(use-package vc-dir
  :defer t
  :config
  (progn
    (defun td/vc-git-command (verb fn)
      (let* ((args (vc-deduce-fileset nil t))
             (backend (car args))
             (files (nth 1 args)))
        (if (eq backend 'Git)
            (progn
              (funcall fn files)
              (message (concat verb " "
                               (number-to-string (length files))
                               " file(s).")))
          (message "Not in a vc git buffer."))))

    (defun td/vc-git-add (&optional revision args comment)
      (interactive "P")
      (td/vc-git-command "Staged" 'vc-git-register))

    (defun td/vc-git-reset (&optional revision args comment)
      (interactive "P")
      (td/vc-git-command
       "Unstaged"
       (lambda (files) (vc-git-command nil 0 files "reset" "-q" "--"))))

    (defun td/vc-git-amend (&optional revision args comment)
      (interactive "P")
      (td/vc-git-command
       "Ammended"
       (lambda (files)
         (vc-git-command nil 0 files
                         "commit" "--amend" "--reuse-message=HEAD"))))

    (defadvice vc-dir-refresh
        (after td/vc-hide-up-to-date-after-refresh activate)
      (vc-dir-hide-up-to-date))

    (bind-keys :map vc-dir-mode-map
               ("r" . vc-revert-buffer)
               ("a" . td/vc-git-add)
               ("u" . td/vc-git-reset)
               ("A" . td/vc-git-amend))

    (bind-keys :map vc-prefix-map
               ("r" . vc-revert-buffer)
               ("a" . td/vc-git-add)
               ("u" . td/vc-git-reset))))

(use-package compile
  :defer t
  :config
  (setq compilation-scroll-output t))

(use-package ivy
  :defer t
  :init (ivy-mode t)
  :config
  (setq ivy-format-function 'ivy-format-function-arrow
        ivy-count-format ""
        ivy-use-virtual-buffers t
        projectile-completion-system 'ivy))

(use-package counsel
  :ensure t
  :defer t
  :bind (([remap find-file] . counsel-find-file)
         ("M-m" . counsel-M-x)
         ("C-c i" . counsel-imenu)))

(use-package imenu
  :defer t
  :config
  (setq imenu-auto-rescan t))

(use-package haml-mode
  :ensure t
  :defer t
  :mode ("\\.haml" . haml-mode))

(use-package indent-guide
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'haml-mode-hook #'indent-guide-mode)
    (add-hook 'python-mode-hook #'indent-guide-mode)))

(use-package ibuffer
  :defer t
  :bind ([remap list-buffers] . ibuffer))

;; (require 'typewriter-mode)
;; (turn-on-typewriter-mode)

(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode t))

(provide 'init)
;;; init.el ends here
