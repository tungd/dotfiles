
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'use-package)

;;;;
;; (defvar td/data-directory (expand-file-name "data/" user-emacs-directory))
(defvar td/data-directory "/tmp/")

;; I rarely turn off the computer, so it's ok to have these at /tmp.
;; You know, auto cleanup as a service ;)
(setq backup-directory-alist `((".*" . ,td/data-directory))
      auto-save-list-file-prefix td/data-directory
      auto-save-timeout (* 5 60)
      create-lockfiles nil)

;;
(setq user-full-name "Tung Dao"
      user-mail-address "me@tungdao.com"
      default-input-method 'vietnamese-telex)

(setq default-frame-alist
      '((left-fringe . 8) (right-fringe . 4)
        ;; (font . "Fira Mono 14")
        (left . 256)
        (width . 120) (height . 34)
        (border-width . 0)
        (internal-border-width . 0)))

(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(setq-default indent-tabs-mode nil)
(setq require-final-newline t) ; auto-insert final newlines in all files

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
          tramp-debug-buffer t
          tramp-default-method "ftp")

    (add-to-list 'auth-sources "~/.emacs.d/authinfo.gpg")
    (setq ange-ftp-netrc-filename "~/.emacs.d/authinfo.gpg")))

;;
(setq visible-bell nil
      inhibit-startup-screen t
      delete-by-moving-to-trash t
      custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Never require full word answers
(defalias 'yes-or-no-p 'y-or-n-p)

(load custom-file :no-error)
(use-package solarized-theme
  :ensure t
  :init
  (progn
    (setq solarized-scale-org-headlines nil)
    (load-theme 'solarized-dark)))

(blink-cursor-mode -1)
(electric-pair-mode t)
(column-number-mode t)
(recentf-mode t)
(pending-delete-mode t)

(use-package isearch
  :bind (([remap isearch-forward] . isearch-forward-regexp)
         ([remap isearch-backward] . isearch-backward-regexp))
  :config (setq lazy-highlight-initial-delay 0)
  ;; :init
  ;; (progn
  ;;   (defun td/isearch-message (&optional c-q-hack ellipsis)
  ;;     "Cursor flashing in the echo area makes me crazy."
  ;;     (isearch-message c-q-hack nil))
  ;;   (setq lazy-highlight-initial-delay 0
  ;;         isearch-message-function #'td/isearch-message)
  ;;   (require 'isearch-diacritics-fold))
  )

(use-package exec-path-from-shell
  :ensure t
  :defer t
  :init (exec-path-from-shell-initialize))

(use-package ido
  :defer t
  :init (ido-mode t)
  :config
  (setq resize-mini-window nil
        ido-everywhere t
        ido-enable-flex-matching t
        ido-use-virtual-buffers 'auto))

(use-package ido-ubiquitous
  :ensure t
  :defer t
  :init (ido-ubiquitous-mode t))

(use-package smex
  :ensure t
  :defer t
  :init (smex-initialize)
  :bind (("M-m" . smex)
         ("M-M" . smex-major-mode-commands)))

(use-package company
  :ensure t
  :defer t
  :bind ("M-/" . company-complete-common-or-cycle)
  :init (global-company-mode t)
  :config
  (progn
    (setq company-idle-delay 0
          company-minimum-prefix-length 4
          company-backends
          '(company-css
            company-capf
            (company-dabbrev-code company-gtags company-etags company-keywords)
            company-dabbrev
            company-files))

    (bind-keys :map company-active-map
               ("C-n" . company-select-next-or-abort)
               ("C-p" . company-select-previous-or-abort))))

(use-package savehist
  :defer t
  :init (savehist-mode t)
  :config
  (setq savehist-file (expand-file-name "savehist" td/data-directory)))

(use-package anzu
  :ensure t
  :defer t
  :init (global-anzu-mode t))

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
  :bind ("C-." . ace-jump-mode))

(use-package projectile
  :ensure t
  :defer t
  :init (projectile-global-mode t))

(use-package diff-hl
  :ensure t
  :defer t
  :init (global-diff-hl-mode)
  :config
  (progn
    (setq diff-hl-side 'right
          diff-hl-draw-borders nil)

    (defun diff-hl-overlay-modified (ov after-p beg end &optional len)
      "Markers disappear and reapear is kind of annoying to me.")))

(use-package magit
  :ensure t
  :defer t)

(use-package smart-mode-line
  :ensure t
  :defer t
  :init (sml/setup))

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

(use-package web-mode
  :ensure t
  :defer t
  :mode (("\\.html" . web-mode)
         ("\\.jsx" . web-mode))
  :init (add-hook 'web-mode-hook #'emmet-mode)
  :config (setq web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2))

(use-package js
  :mode (("\\.json$" . js-mode))
  :config
  (setq js-indent-level 2))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode))
  :config (setq js2-basic-offset 2))

(use-package clojure-mode
  :ensure t
  :defer t
  :mode (("\\.clj$" . clojure-mode)
         ("\\.cljs$" . clojure-mode)
         ("build\\.boot$" . clojure-mode)))

;;;;

(defun td/next-ten-visual-line ()
  (interactive)
  (next-logical-line 10))

(defun td/previous-ten-visual-line ()
  (interactive)
  (next-logical-line -10))

(bind-keys ("M-n" . td/next-ten-visual-line)
           ("M-p" . td/previous-ten-visual-line))
;; (use-package outline
;;   :defer t
;;   :bind (("M-n" . outline-next-heading)
;;          ("M-p" . outline-previous-heading)))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun td/join-next-line ()
  (interactive)
  (join-line -1))

(bind-key "M-J" #'td/join-next-line)

(defun td/cleanup-buffer ()
  (interactive)
  (save-excursion
    (whitespace-cleanup-region (point-min) (point-max))
    (indent-region (point-min) (point-max))))

(bind-key "M-=" #'td/cleanup-buffer)
