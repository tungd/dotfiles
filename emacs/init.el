;;; init.el --- Tung Dao's Emacs Setup -*- lexical-binding: t; -*-

;; So, as an effort to improve the responsiveness, I'm going to reboot my Emacs
;; configuration. Hope it is better this time.
;;
;; The idea is that I'm going to add things in, bit by bit, just enough to get it
;; going. This way I will be able to nail down the packages that cause issue, and
;; look for alternatives.

;;; Enable Lexical Binding
;; Make things a little bit faster. For context: https://www.emacswiki.org/emacs/DynamicBindingVsLexicalBinding
(require 'subr-x)
(setq load-prefer-newer t)

;;; Packages and initialization

;; All the packages I used are from ELPA archives. However, I install them
;; automatically with =use-package.el= instead of using =package.el= directly.
;; Since Emacs 29.1, =use-package.el= has been bundled with Emacs.

;; Also, Emacs 29.1 added the =package-vc-install= command, which is really handy as
;; quite a few of the packages I used are not available on MELPA yet.

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
  :custom
  (package-quickstart t))

(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(use-package bind-key
  :functions (override-global-mode bind-key--remove)
  :hook (after-init . override-global-mode))

(defconst td/extra-exec-path
  '("/opt/local/bin"
    "/opt/local/sbin"
    "/usr/local/bin"
    "~/Library/Python/3.14/bin"
    "~/Library/pnpm"
    "~/.local/bin"
    "~/.opam/default/bin"
    "~/.claude/local")
  "Stable executable directories to prepend to Emacs process search paths.")

(let ((extra-exec-path
       (delq nil
             (mapcar (lambda (path)
                       (let ((expanded (directory-file-name
                                        (expand-file-name path))))
                         (when (file-directory-p expanded) expanded)))
                     td/extra-exec-path))))
  (setq exec-path (delete-dups (append extra-exec-path exec-path)))
  (setenv "PATH"
          (string-join
           (delete-dups
            (append extra-exec-path
                    (split-string (or (getenv "PATH") "") path-separator t)))
           path-separator)))

;;; Defaults

;; Personal information that some package use:

(setopt user-full-name "Tung Dao"
        user-mail-address "me@tungdao.com"
        default-input-method "vietnamese-telex")
(defvar personal-keybindings nil)

;; Less verbose choice:

(setq use-short-answers t
      history-delete-duplicates t)

;; Sane, modern defaults:

;; - clean minimal UI
;; - no unnecessary temporary files (I do keep backup files, but not auto-save
;; and lock files)

(setopt auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t))
      backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups")))
      create-lockfiles nil
      remote-file-name-inhibit-locks t
      remote-file-name-inhibit-auto-save-visited t)

;; Performance stuffs, I'm not sure how relevant these are, since I've moved to =nativecomp=.

(setq-default
 bidi-display-reordering nil
 bidi-paragraph-direction 'left-to-right
 bidi-inhibit-bpa t)

(setq read-process-output-max (* 16 1024 1024)
      inhibit-compacting-font-caches t)

(savehist-mode t)

(setopt echo-keystrokes 0.02)

(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay most-positive-fixnum)
  (which-key-idle-secondary-delay 1e-100))

(setopt enable-recursive-minibuffers t)

(use-package eldoc
  :config
  (global-eldoc-mode -1))

;;;; Scratch Buffer

(setopt initial-scratch-message nil
        initial-major-mode 'org-mode)

;;;; Server
(use-package server
  :hook (after-init . server-start))

;;;; MacOS specific stuffs
(if (boundp 'ns-command-modifier)
    (setopt ns-command-modifier 'meta))
(if (boundp 'ns-option-modifier)
    (setopt ns-option-modifier 'super))
(if (boundp 'ns-use-native-fullscreen)
    (setopt ns-use-native-fullscreen t))

(setopt trash-directory "~/.Trash"
      delete-by-moving-to-trash t)

;;; Navigation

;;;; File position
(use-package saveplace
  :hook (after-init . save-place-mode)
  :custom
  (save-place-autosave-interval 300))

;;;; Projects
(use-package files
  :custom
  (remote-file-name-access-timeout 5)
  (find-file-visit-truename t))

;; Emacs built-in =project.el= has gone a long way so I'm using that now. There are
;; still some missing features coming from =projectile=, but I can live with that.

(use-package magit-extras
  :functions (magit-project-status))

(use-package project
  :commands (project-find-file project-vc-dir project-current)
  :custom
  (project-file-history-behavior 'relativize)
  (project-switch-commands 'magit-project-status)
  :config
  (autoload 'magit-project-status "magit-extras" nil t)
  (keymap-set project-prefix-map "m" #'magit-project-status))

;;;; Symbols

;; Use `xref' as the common jump interface, with Citre providing a tags backend.

;; Default key bindings is

;; | Binding   | Command                              | Description |
;; |-----------+--------------------------------------+-------------|
;; | =M-.=     | =xref-find-definitions=              |             |
;; | =C-M-p=   | =xref-find-definitions-other-window= |             |
;; | =C-M-g=   | =xref-pop-marker-stack=              |             |

(use-package grep
  :bind ("C-c s" . grep)
  :custom
  (grep-command "rg -nS --no-heading ")
  (grep-use-null-device nil))

(use-package xref
  :bind (("C-M-p" . xref-find-definitions-other-window)
         ("C-M-g" . xref-go-back))
  :hook ((xref-after-update . outline-minor-mode))
  :custom
  (xref-search-program 'ripgrep))

(defun td/citre-project-root ()
  "Return the current project root for Citre."
  (when-let* ((project (project-current nil)))
    (project-root project)))

(use-package citre
  :ensure t
  :defer t
  :functions (citre-raven-create-tags-file
              citre-raven-update-this-tags-file)
  :init
  (require 'citre-config)
  :bind (("C-c t j" . citre-jump)
         ("C-c t J" . citre-jump-back)
         ("C-c t p" . citre-ace-peek))
  :custom
  (citre-project-root-function #'td/citre-project-root)
  (citre-default-create-tags-file-location 'global-cache)
  (citre-edit-ctags-options-manually nil)
  (citre-auto-enable-citre-mode-modes '(prog-mode))
  (citre-auto-enable-citre-mode-backends-for-remote nil)
  (citre-readtags-program
   (or (executable-find "ureadtags")
       (executable-find "readtags")
       "readtags"))
  :config
  (require 'citre-raven)
  (bind-key "C-c t c" #'citre-raven-create-tags-file)
  (bind-key "C-c t u" #'citre-raven-update-this-tags-file))

;;;; Mini-buffer

;; Shortcut key to go back to the home directory, works regardless of the
;; mini-buffer completion framework.

(use-package minibuffer
  :custom
  (minibuffer-visible-completions 'up-down)
  (completion-eager-display 'auto)
  (completion-eager-update 'auto)
  (completion-extra-properties '(:eager-display t :eager-update t))
  (completion-auto-help nil)
  (completion-show-help nil)
  (completion-auto-select 'second-tab)
  (completion-cycle-threshold 3)
  (completions-sort #'prescient-completion-sort)
  (completions-max-height 16)
  (completions-format 'one-column)
  (completions-detailed t)
  (completions-group t))

(bind-key "TAB" #'minibuffer-complete minibuffer-mode-map)

(bind-key "C-p" #'minibuffer-previous-completion minibuffer-local-map)
(bind-key "C-n" #'minibuffer-next-completion minibuffer-local-map)

(bind-key "C-p" #'minibuffer-previous-completion completion-in-region-mode-map)
(bind-key "C-n" #'minibuffer-next-completion completion-in-region-mode-map)

(ido-mode -1)

(use-package prescient
  :ensure t
  :hook (after-init . prescient-persist-mode)
  :preface
  (defun td/set-completion-category-overrides (category overrides)
    "Merge OVERRIDES into completion CATEGORY overrides."
    (let ((current (copy-tree (alist-get category completion-category-overrides))))
      (dolist (entry overrides)
        (setf (alist-get (car entry) current) (cdr entry)))
      (setf (alist-get category completion-category-overrides) current)))
  :init
  (add-to-list 'completion-styles 'prescient)
  ;; `M-x` uses a large `command` category. Keep prescient matching/sorting
  ;; there, but avoid eager display so command completion stays explicit.
  (td/set-completion-category-overrides
   'command
   '((eager-display . nil)
     (eager-update . nil)
     (styles . (prescient basic))
     (display-sort-function . prescient-completion-sort)
     (cycle . t)))
  ;; Keep recency/frequency ahead of the old shortest-first behavior.
  (setq prescient-sort-length-enable nil
        prescient-aggressive-file-save t))

(defun td/minibuffer-smart-tilde ()
  (interactive)
  (if (not (looking-back "/" 0))
      (call-interactively 'self-insert-command)
    (beginning-of-line)
    (kill-line)
    (insert "~/")))

(bind-key "~" #'td/minibuffer-smart-tilde minibuffer-local-map)
(bind-key "<s-backspace>" #'backward-kill-word minibuffer-local-map)

(defun td/isearch-consult-ripgrep ()
  "Launch consult-ripgrep with current search term.
Uses project root if in a project, otherwise current directory."
  (interactive)
  (let* ((search-term (if isearch-string
                          isearch-string
                        (thing-at-point 'symbol t)))
         (current-proj (project-current))
         (search-dir (if current-proj
                         (project-root current-proj)
                       default-directory)))
    (isearch-exit)
    (consult-ripgrep search-dir search-term)))

(use-package consult
  :ensure t
  :bind (("C-M-l" . consult-line)
         ("C-M-j" . consult-buffer)
         ("M-g b" . consult-bookmark)
         ("M-g y" . consult-yank-from-kill-ring)
         ("M-g t" . consult-theme)
         ("M-g m" . consult-mode-command)
         ("M-g r" . consult-ripgrep)
         ("M-g o" . consult-outline)
         ([remap goto-line] . consult-goto-line)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap imenu] . consult-imenu)
         :map isearch-mode-map
         ("C-i" . td/isearch-consult-ripgrep))
  :custom
  (consult-narrow-key (kbd "<"))
  (consult-project-root-function #'vc-root-dir)
  (consult-preview-key nil)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package consult-flymake
  :bind ("M-g e" . consult-flymake))

(use-package recentf
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 256)
  (recentf-autosave-interval 300)
  (recentf-auto-cleanup 'never)
  :config
  (add-to-list 'recentf-exclude #'file-remote-p)
  (add-to-list 'recentf-exclude "elpa/.*")
  (add-to-list 'recentf-exclude "__init__.py")
  (add-to-list 'recentf-exclude "_build/*")
  (add-to-list 'recentf-exclude "node_modules/.*"))

(bind-key* "C-;" #'execute-extended-command)

(defvar td/leader-map (make-sparse-keymap)
  "Personal prefix map used from `C-l'.")
(set-keymap-parent td/leader-map ctl-x-map)
(global-set-key (kbd "C-l") td/leader-map)
(bind-key* "C-l" td/leader-map)

;; Use consult for completion-in-region (more efficient than default)
(setopt completion-in-region-function #'consult-completion-in-region)

;;;; Bookmark
(use-package bookmark
  :custom
  (bookmark-save-flag 1))

;;; Window Management

;; Temporary "focus" on a buffer by maximizing it in the current frame.

(defun td/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(bind-key* [remap delete-other-windows] #'td/toggle-maximize-buffer)
(bind-key* "M-C-o" #'td/toggle-maximize-buffer)
(bind-key* "M-o" #'other-window)

;; Buffer location customization

(use-package window
  :custom
  (split-height-threshold nil)
  (split-window-preferred-direction 'horizontal)
  :config
  ;; (add-to-list 'display-buffer-alist
  ;;              '("^\\*codex:"
  ;;                (display-buffer-in-side-window)
  ;;                (side . right)
  ;;                (window-width . 0.32)))
  ;; (add-to-list 'display-buffer-alist
  ;;              '("^\\*claude:"
  ;;                (display-buffer-in-direction)
  ;;                (direction . right)
  ;;                (window-width . 85)))
  (add-to-list 'display-buffer-alist
               '("\\*Warnings\\*" display-buffer-in-direction
                 (direction . bottom)
                 (window-height . 8)))
  (add-to-list 'display-buffer-alist
               '("\\*Help\\*"
                 (display-buffer-reuse-window display-buffer-pop-up-window)
                 (inhibit-same-window . t)))
  (add-to-list 'display-buffer-alist
               '("\\*Org-Babel Error Output\\*" display-buffer-in-direction
                 (direction . bottom)
                 (window-height . 8))))

(use-package winner
  :hook (after-init . winner-mode))

;;; General Editing
(use-package editorconfig
  :hook (after-init . editorconfig-mode))

(use-package vundo :ensure t)

(bind-key [remap zap-to-char] #'zap-up-to-char)

(use-package misc
  :custom
  (duplicate-line-final-position -1)
  :bind*
  ("C-c C-d" . duplicate-dwim))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package ibuffer
  :defer t
  :bind ([remap list-buffers] . ibuffer)
  :custom
  (ibuffer-saved-filter-groups
   '(("default"
      ("Claude" (name . "^\\*claude:"))
      ("Org" (mode . org-mode))
      ("Dired" (mode . dired-mode))
      ("Special" (name . "^\\*.*\\*$")))))
  :hook (ibuffer-mode . (lambda () (ibuffer-switch-to-saved-filter-groups "default"))))

(setopt kill-do-not-save-duplicates t)

;; Basic settings:

  (setq-default
   tab-width 2
   indent-tabs-mode nil
   reb-re-syntax 'string)

;; Editing utilities:

(use-package crux
  :ensure t
  :bind (;; There's a built-in `switch-to-prev-buffer', but it is less helpful
           ;; since it doesn't allow me to quickly switch between the most
           ;; recent buffers
         ("M-C-]" . crux-switch-to-previous-buffer)
         ("M-J" . join-line)
         ("M-=" . crux-cleanup-buffer-or-region)
         ("C-M-k" . crux-kill-whole-line)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c C-o" . crux-open-with))
  :config
    ;(crux-with-region-or-buffer indent-region)
    ;(crux-with-region-or-buffer untabify)
    ;(crux-with-region-or-point-to-eol kill-ring-save)
  (setopt kill-do-not-save-duplicates t))

(bind-key* "C-x C-k" #'kill-current-buffer)
(bind-key* "C-c r" #'rename-visited-file)
(bind-key* [remap kill-line] #'crux-smart-kill-line)
(bind-key* "s-n" #'next-buffer)
(bind-key* "s-p" #'previous-buffer)

;; Make the file executable if starting with "shebang":

(defun td/byte-compile-user-emacs-file-after-save ()
  "Byte-compile `init.el' and files under `user-lisp-directory'."
  (when (and buffer-file-name
             (derived-mode-p 'emacs-lisp-mode)
             (string-suffix-p ".el" buffer-file-name)
             (or (equal (file-truename buffer-file-name)
                        (file-truename (expand-file-name "init.el" user-emacs-directory)))
                 (and (boundp 'user-lisp-directory)
                      (file-in-directory-p buffer-file-name user-lisp-directory)))
             (not (string-prefix-p "." (file-name-nondirectory buffer-file-name))))
    (condition-case err
        (byte-compile-file buffer-file-name)
      (error
       (message "Byte compile failed for %s: %s"
                (file-name-nondirectory buffer-file-name)
                (error-message-string err))))))

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
(add-hook 'after-save-hook #'td/byte-compile-user-emacs-file-after-save)

;;;; Search and replace
(use-package isearch
  :custom
  (isearch-wrap-pause 'no)
  (isearch-lazy-count t)
  (search-ring-max 256)
  (regexp-search-ring-max 200)
  (search-allow-motion t)
  (isearch-motion-changes-direction t)
  :bind
  (([remap isearch-forward] . isearch-forward-regexp)
   ([remap isearch-backward] . isearch-backward-regexp)))

(use-package visual-replace
  :ensure t
  :bind (("M-r" . visual-replace)
         ([remap query-replace] . visual-replace)
         :map isearch-mode-map
         ("M-r" . visual-replace-from-isearch))
  :custom
  (visual-replace-default-to-full-scope t)
  (visual-replace-display-total t)
  (visual-replace-keep-initial-position t)
  :config
  (define-key visual-replace-mode-map (kbd "M-r")
              visual-replace-secondary-mode-map)
  (unbind-key [remap yank] visual-replace-mode-map))

(use-package replace
  :bind (:map isearch-mode-map ("C-o" . isearch-occur))
  :config
  (add-to-list 'display-buffer-alist
               '("\\*Occur\\*"
                 (display-buffer-reuse-mode-window display-buffer-below-selected)
                 (dedicated . t)
                 (post-command-select-window . t)
                 (window-height . 20))))

(use-package avy
  :ensure t
  :bind (:map isearch-mode-map
              ("C-j" . avy-isearch))
  :bind* ("C-'" . avy-goto-char-timer)
  :custom
  (avy-background t)
  (avy-all-windows t))

;;;; Long lines

;; Long lines are annoying. Auto wrap all texts at 80.

(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-avoid-polling t)
  (auto-revert-interval 5)
  (auto-revert-check-vc-info nil))

(setq-default
 comment-auto-fill-only-comments t
 fill-column 80)

(add-hook 'text-mode-hook #'turn-on-auto-fill)

;;;; Whitespace

;; Cleanup whitespaces automatically on save.

(use-package whitespace
  :commands (whitespace-cleanup)
  :hook (before-save . whitespace-cleanup)
  :config
    ;; (setopt whitespace-style (remove 'newline-mark whitespace-style))
  )

;;;; Parenthesis

;; Parenthesis come in pairs, that's why they are cumbersome to deal with.

(use-package paren
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-delay 0)
  (show-paren-not-in-comments-or-strings t)
  (show-paren-context-when-offscreen 'overlay))

(use-package elec-pair
  :hook (after-init . electric-pair-mode))

(use-package surround
  :ensure t
  :bind-keymap ("C-M-s" . surround-keymap))

(defun td/mark-line-dwim ()
  (interactive)
  (call-interactively #'beginning-of-line)
  (call-interactively #'set-mark-command)
  (call-interactively #'end-of-line))

(bind-key "M-C-SPC" #'td/mark-line-dwim)

(use-package delsel
  :hook (after-init . delete-selection-mode))

;;;; Alignment
(use-package align
  :bind (("C-c =" . align))
  :config
  (add-to-list 'align-rules-list
               '(js-object-props
                 (modes . '(js-mode js2-mode js-ts-mode tsx-ts-mode))
                 (regexp . "\\(\\s-*\\):")
                 (spacing . 0)))
  (add-to-list 'align-rules-list
               '(css-declaration
                 (modes . '(css-mode css-ts-mode))
                 (regexp . "^\\s-*\\w+:\\(\\s-*\\).*;")
                 (group 1)))
  (add-to-list 'align-rules-list
               '(haskell-record-fields
                 (modes . '(haskell-mode))
                 (regexp . "\\(\\s-*\\)::")
                 (spacing . 1)))
  (add-to-list 'align-rules-list
               '(haskell-aeson-fields
                 (modes . '(haskell-mode))
                 (regexp . "\\(\\s-*\\).=")
                 (spacing . 1))))

;;;; Diff
(use-package ediff
  :defer t
  :custom
  (ediff-keep-variants nil)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))

;;; Shell and remote
(use-package envrc
  :ensure t
  :custom
  (envrc-direnv-executable (or (executable-find "direnv") "direnv"))
  :hook (after-init . envrc-global-mode))

(use-package comint
  :bind ("C-c C-l" . comint-clear-buffer)
  :custom
  (comint-terminfo-terminal "dumb-emacs-ansi"))

;; tterm - OCaml-based terminal emulator (local development)
(defconst td/tterm-source-directory "/Users/tung/Projects/tungd/tterm"
  "Local tterm checkout used by this Emacs configuration.")

(add-to-list 'load-path td/tterm-source-directory)
(autoload 'tterm "tterm" nil t)
(autoload 'tterm-dashboard "tterm-dashboard" nil t)
(autoload 'tterm-send-file "tterm" nil t)

(use-package tterm
  :defer t
  :custom
  (tterm-module-path
   (expand-file-name "tterm-module.so" td/tterm-source-directory))
  (tterm-buffer-title-function #'tterm-buffer-title-collapse-parents)
  (tterm-osc-52-policy 'confirm)
  :config
  (defun td/tterm-reload ()
    "Reload the local tterm checkout and reset its keymaps."
    (interactive)
    (dolist (symbol '(tterm-mode-map tterm--char-mode-map tterm-copy-mode-map))
      (when (boundp symbol)
        (makunbound symbol)))
    (load-file
     (expand-file-name "tterm.el" td/tterm-source-directory))
    (when (fboundp 'tterm-redraw-all)
      (tterm-redraw-all))))

(use-package tterm-consult
  :after (consult tterm)
  :config
  (tterm-consult-register-source))

;; scv - Emacs session integration (local development)
(use-package scv
  :load-path "~/Projects/tungd/scv/emacs"
  :defer t
  :commands (scv-menu
             scv-new-session
             scv-session-manager
             scv-open-session-viewer
             scv-open-prompt-draft)
  :custom
  (scv-source-directory (expand-file-name "~/Projects/tungd/scv/"))
  (scv-executable (expand-file-name "~/.local/bin/scv")))

;;;; Tramp
(use-package tramp
  :custom
  (tramp-allow-unsafe-temporary-files t)
  (tramp-default-method "ssh")
  (tramp-use-scp-direct-remote-copying t)
  (tramp-copy-size-limit (* 1024 1024))
  (tramp-verbose 2)
  :config
  (add-to-list 'auth-sources (expand-file-name "authinfo.gpg" user-emacs-directory))
  (add-to-list 'auth-sources 'macos-keychain-generic t)
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process))

;; Some speedup for Tramp:

(use-package vc
  :custom
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git))
  (vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp)))

;;; Programming

;; Native LSP support via =Eglot= since Emacs 29.1 (disabled - using tags + Claude instead)

;; I'm experimenting with working purely with LLM and without LSP. It's not that I don't like LSP, it's just that I don't think it's efficient, especially in the era of LLM.

;; (use-package swift-mode
;;   :ensure t)

;; Keep Emacs Lisp outlines focused on comment headings so `consult-outline'
;; navigates section markers instead of every top-level form.
(defun td/emacs-lisp-outline-level ()
  "Return the outline level for an Emacs Lisp comment heading."
  (- (match-end 0) (match-beginning 0) 2))

(defun td/setup-emacs-lisp-outline ()
  "Use comment-only outlines in `emacs-lisp-mode' buffers."
  (setq-local outline-regexp ";;;+ ")
  (setq-local outline-level #'td/emacs-lisp-outline-level))

(add-hook 'emacs-lisp-mode-hook #'td/setup-emacs-lisp-outline)

;; Native Tree-sitter support since Emacs 29.

(defconst td/treesit-grammar-directory
  (expand-file-name "tree-sitter/" user-emacs-directory)
  "Directory for locally built tree-sitter grammars.")

(defconst td/macports-treesit-grammar-directory
  "/opt/local/lib/"
  "Directory for MacPorts-provided tree-sitter grammars.")

(defconst td/treesit-language-source-alist
  '((kotlin "https://github.com/fwcd/tree-sitter-kotlin.git"
            :commit "0.3.8")
    (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
              :commit "v0.5.3"
              :source-dir "tree-sitter-markdown/src")
    (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                     :commit "v0.5.3"
                     :source-dir "tree-sitter-markdown-inline/src")
    (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml"
           :commit "v0.24.2"
           :source-dir "grammars/ocaml/src")
    (ocaml-interface "https://github.com/tree-sitter/tree-sitter-ocaml"
                     :commit "v0.24.2"
                     :source-dir "grammars/interface/src")
    (protobuf "https://github.com/casouri/tree-sitter-module.git"
              :commit "v2.5")
    (swift "https://github.com/alex-pinkus/tree-sitter-swift.git"
           :commit "0.7.2-with-generated-files"))
  "Pinned tree-sitter grammar recipes used by this configuration.")

(defun td/bootstrap-treesit-grammars (&optional force)
  "Install missing tree-sitter grammars into `td/treesit-grammar-directory'.
With prefix argument FORCE, rebuild every configured grammar."
  (interactive "P")
  (require 'treesit)
  (make-directory td/treesit-grammar-directory t)
  (let ((treesit-language-source-alist td/treesit-language-source-alist))
    (dolist (language (mapcar #'car treesit-language-source-alist))
      (when (or force
                (not (treesit-language-available-p language)))
        (treesit-install-language-grammar
         language
         td/treesit-grammar-directory)))))

(use-package treesit
  :custom
  (treesit-extra-load-path (list td/treesit-grammar-directory
                                 td/macports-treesit-grammar-directory))
  (treesit-auto-install-grammar 'ask)
  :config
  (setq treesit-language-source-alist td/treesit-language-source-alist))

(use-package expreg
  :ensure t
  :custom
  (expreg-restore-point-on-quit t)
  :bind (("M--" . expreg-expand)
         ("M-_" . expreg-contract)))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

(use-package go-ts-mode
  :mode (("go.mod$" . go-mod-ts-mode)
         ("\\.go\\'" . go-ts-mode))
  :custom
  (go-ts-mode-indent-offset 2))

(setopt
 major-mode-remap-alist
 '((js-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (markdown-mode . markdown-ts-mode)
   (gfm-mode . markdown-ts-mode)
     ;; (python-mode . python-ts-mode)
   ))

;;;; Auto completion

;; I use auto completion sparingly.

(use-package dabbrev
  :custom (dabbrev-case-fold-search nil)
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . completion-at-point)))

(defun td/expand-lines ()
  (interactive)
  (let ((hippie-expand-try-functions-list
         '(try-expand-line)))
    (call-interactively 'hippie-expand)))

(bind-key "C-x C-l" #'td/expand-lines)

;; Cloudflare AI Gateway via its OpenAI-compatible endpoint.

(use-package gptel-cloudflare-ai-gateway
  :functions (gptel-cloudflare-ai-gateway-setup))

(use-package gptel
  :ensure t
  :bind ("C-l a" . gptel-menu)
  :hook (gptel-mode . visual-line-mode)
  :config
  (require 'gptel-org)
  (require 'gptel-cloudflare-ai-gateway)
  (gptel-cloudflare-ai-gateway-setup)
  (setopt
   gptel-default-mode 'org-mode
   gptel-include-reasoning 'ignore
   gptel-model 'workers-ai/@cf/google/gemma-4-26b-a4b-it))

;;;; Error checking
(use-package flymake
  :defer t
  :bind (:map flymake-mode-map
              ("C-c e n" . flymake-goto-next-error)
              ("C-c e p" . flymake-goto-prev-error)))

;;;; Version Control

;; Git has won the version control war, everyone uses Git now. Emacs'
;; built-in VC has great support for git but Magit is godsend.

(use-package magit
  :ensure t
  :bind ("C-x p v" . magit)
  :custom
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (magit-show-long-lines-warning nil)
  :config
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff))

;;;; Compile

;; I use =compile= not only for compilation but also as a generic method to run
;; repetitive tasks. For example, I to run unit tests repeatedly, I first run
;; =M-x compile= with the test commands. Subsequence =recompile= call will
;; re-run the tests.

(use-package compile
  :bind ("C-c m" . recompile)
  :hook (compilation-filter . ansi-color-compilation-filter)
  :hook (compilation-mode . visual-line-mode)
  :custom
  (compilation-ask-about-save nil)
  (compilation-scroll-output t)
  :config
  (remove-hook 'compilation-mode-hook 'tramp-compile-disable-ssh-controlmaster-options))

;;;; Code folding
(use-package treesit-fold
  :ensure t
  :hook ((after-init . global-treesit-fold-mode)
           ;(after-init . global-treesit-fold-indicators-mode)
         (treesit-fold-mode . treesit-fold-line-comment-mode))
  :config
  (define-fringe-bitmap 'treesit-fold-indicators-fr-plus
    [#b01100000
     #b00110000
     #b00011000
     #b00001100
     #b00011000
     #b00110000
     #b01100000
     #b00000000])
  (define-fringe-bitmap 'treesit-fold-indicators-fr-minus-tail
    [#b00000000
     #b10000010
     #b11000110
     #b01101100
     #b00111000
     #b00010000
     #b00000000
     #b00000000])
  (define-fringe-bitmap 'treesit-fold-indicators-fr-center
    (vector #b00000000))
  (define-fringe-bitmap 'treesit-fold-indicators-fr-end-left
    (vector #b00000000)))

;;;; Web Development
(defun td/format-html-attributes ()
  (interactive)
  (save-excursion
    (re-search-backward "<")
    (while (not (looking-at "[\n\r/]"))
      (re-search-forward "\s+[^=]+=")
      (goto-char (match-beginning 0))
      (newline-and-indent))))

(bind-key "C-M-=" #'td/format-html-attributes)

(use-package emmet-mode
  :ensure t
  :hook (mhtml-mode . emmet-mode)
  :bind ("C-M-<return>" . emmet-expand-line)
  :config
  (unbind-key "C-j" emmet-mode-keymap))

(use-package sgml-mode
  :mode (("\\.svg" . sgml-mode)))

;;;; CSS
(use-package css-mode
  :mode ("\\.css\\'" . css-ts-mode)
  :custom
  (css-indent-offset 2))

;;;; JavaScript

;; Like most people I used to use =js2-mode= for all my JavaScript editing,
;; including JSX. Since I'm no longer write as much JavaScript, and I will use
;; =es-lint= for syntax checking anyways, I think I'm going to give the built-in
;; =js-mode= a try.

(use-package js
  :mode (("\\.eslintrc$" . json-ts-mode))
  :mode (("\\.mjs$" . js-ts-mode))
  :custom
  (js-indent-level 2)
  (js-indent-first-init 'dynamic)
  (js-switch-indent-offset 2)
  (js-enabled-frameworks '(javascript)))

;;;; OCaml

;; I'm a Python veteran. When I have the opportunity to, I tried to use
;; Haskell. Recently I have been looking into OCaml, it seems like a very good,
;; practical choice.

;; The following are the issues I have working in Python and Haskell, they are the
;; reason I'm considering OCaml as my main language. Hopefully I'll get a better
;; experience with OCaml. Besides the fact that OCaml is strongly-typed and can be
;; used for both the web and server, following are my bad experiences with either
;; Python or Haskell:

;; 1. Python:
;; - No good package manager: poetry used to be the silver bullet, combining
;; =pyenv= and =pipenv=, while also fixing their issues. For what it's worth,
;; Poetry is miles better than the previous solutions, yet it still suffers
;; from problems that are unbearable for me.
;; - The lack of types. That alone is a serious drawback for me. Sure I can add
;; type annotations and use mypy, but unless libraries are also shipped with
;; type definitions, those provides very limited guarantee, which defeats the
;; purpose of having types in the first place.
;; - Library breaking changes: cryptonite changed and broke my code producing
;; APNS push packages. It can't be detected until it's shipped to production,
;; so it's really bad.

;; 2. Haskell
;; - Stack breaks.
;; - The compiler is slow, and there's no good story regarding cross-compile. My
;; guess is that the runtime is so sophisticated that it has to be linked to
;; at least libc, hence making producing static binaries much harder.
;; - Lack of production oriented library/framework. It's kind of like with
;; Clojure, the libraries are there and they are excellent, but there is no
;; standard bundle requiring a lot of wiring setting up a project. OCaml has Sihl.
;; - I was told that OCaml is worse than Haskell regarding libraries, but in my
;; experience that is not true. OCaml might have less libraries, but they are
;; much more comprehensive and well-maintained. A lot of the libraries in the
;; Haskell world seems to be a one-off experiment, or an one-time job then
;; abandoned at best. (I'm talking about iCalendar, and there are many other cases).

;;;; Kotlin
(use-package kotlin-ts-mode
  :ensure t
  :mode (("\\.kt\\'" . kotlin-ts-mode)
         ("\\.kts\\'" . kotlin-ts-mode)))

;;;; Terraform
(use-package terraform-mode
  :ensure t
  :mode (("\\.tf" . terraform-mode))
  :custom (terraform-format-on-save t))

;;;; SQL
(use-package sql
  :custom
  (sql-postgres-login-params
   '((user :default "postgres")
     (database :default "postgres")
     (server :default "localhost")
     (port :default 5432)))
  (sql-mysql-login-params
   '((user :default "root")
     (database :default "mysql")
     (server :default "root")
     (password :default "root")
     (port :default 3306))))

;;;; Misc

;; These are supports for other stuffs that I used:

(use-package yaml-ts-mode
  :mode (("\\.yaml$" . yaml-ts-mode)
         ("\\.yml$" . yaml-ts-mode))
  :hook ((yaml-ts-mode . display-line-numbers-mode)
         (yaml-ts-mode . visual-line-mode)))

;;; Document and management

;; I use Org for almost everything. Blogging, task management, API documentation,
;; literate programming.

;;;; Tracking and tasks management

;; I tried many management tools: Wunderlist, Todoist, Google Calendar
;; .etc. However all of them are missing something really crucial for me. For
;; example Wunderlist has agenda overview, but lacks adding note to
;; tasks. Evernote has execllent note support, but their project management is
;; just barebone, not much than a todo list.

;; Org on the other hand lacks notification and ubiquitous access. I'm looking
;; for a solution though.

;; Here's my basic Org setup:

;; - A default =INBOX.org= in Documents for task capture and project management
;; - Nicer display with inline images
;; - Enable GTD todo keyword sequence and time logging

(defconst td/org-inbox-file (expand-file-name "~/Documents/INBOX.org")
  "Path to the main Org inbox file.")

(defun td/org-open-inbox ()
  "Open the main Org inbox file."
  (interactive)
  (find-file td/org-inbox-file))

(defun td/org-electric-pair ()
  (require 'elec-pair)
  (setq-local
   electric-pair-inhibit-predicate
   `(lambda (c)
      (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))

(use-package org
  :hook ((org-mode . org-indent-mode)
         (org-mode . visual-line-mode)
         (org-mode . td/org-electric-pair))
  :custom
  (org-directory "~/Documents/Journal")
  (org-default-notes-file td/org-inbox-file)
  (org-agenda-files `(,td/org-inbox-file ,org-directory))
  (org-agenda-skip-unavailable-files t)
  (org-hide-leading-stars t)
    ;; (org-refile-targets '(("~/Desktop/archive.org" . (:level . 1))))
  (org-startup-with-inline-images t)
  (org-startup-folded t)
  (org-todo-keywords
   '((sequence "TODO(t@)" "WAITING(w@)" "|" "DONE(d@/!)" "CANCELED(c@)")))
  (org-src-fontify-natively t)

  :config
  (require 'org-tempo)
  (org-clock-persistence-insinuate)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (http . t)
     (ocaml . t)
     (python . t)
     (shell . t)
     (js . t)
     (kotlin . t)
     (sql . t)
     (sql-mode . t))))

(use-package ob-sql-mode
  :ensure t)

(use-package sql-clickhouse
  :defer t)

;; Agenda overview and filtering. Org provides a bunch of quick overviews:

;; | Binding                | Description                                   |
;; |------------------------+-----------------------------------------------|
;; | =C-c o a t=, =C-c o t= | List the TODO items                           |
;; |------------------------+-----------------------------------------------|
;; | =C-c o a #=            | List stuck projects, see =org-stuck-projects= |
;; |------------------------+-----------------------------------------------|
;; | =C-c o a s=            | Search Org headers                            |

;; Stuck projects are:

;; - Top level outlines that have the tag =project=
;; - Without holding state (waiting/done/canceled)
;; - But don't have any todo items

(use-package org-agenda
  :bind (("C-c o a" . org-agenda)
         ("C-c o i" . td/org-open-inbox)
         ("C-c o t" . org-todo-list))
  :custom
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-window-setup 'current-window)
  (org-stuck-projects
   '("+project+LEVEL=1/-WAITING-DONE-CANCELED" ("TODO" "WAITING") nil "")))

;;;; Note taking

;; As stated earlier, I practice GTD. Working projects and new stuffs go to
;; =INBOX.org= file. Old tasks are archived to =archive.org=. Here's my
;; =org-capture= templates to dump stuffs to =inbox/note=

(use-package org-capture
  :bind* (("C-c o c" . org-capture))
  :custom
  (org-capture-templates
   `(("t" "Inbox item" entry
      (file ,td/org-inbox-file) nil)
     ("l" "TIL" entry
      (file+olp+datetree ,td/org-inbox-file "TIL") nil
      :jump-to-captured t)
     ("b" "Blog" entry
      (file+olp+datetree ,td/org-inbox-file "Blog") nil
      :jump-to-captured t))))

;;;; Literate programming

;; Org Babel for literate programming and API documentation.

(declare-function org-link-preview-region "ol" (&optional include-linked refresh beg end))

(defun td/org-babel-preview-inline-images ()
  "Refresh Org inline image previews after Babel execution."
  (if (fboundp 'org-link-preview-region)
      (org-link-preview-region nil t (point-min) (point-max))
    (funcall (intern "org-display-inline-images"))))

(use-package ob-core
  :defer t
  :hook (org-babel-after-execute . td/org-babel-preview-inline-images)
  :custom
  (org-confirm-babel-evaluate nil))

(use-package ob-http
  :ensure nil
  :defer t
  :commands (td-ob-http-copy-as-curl
             td-ob-http-copy-as-fetch
             td-ob-http-copy-as-python
             td-ob-http-cancel-at-point
             td-ob-http-export-postman-collection
             td-ob-http-import-postman-collection)
  :custom
  (td-ob-http-async-by-default t)
  (td-ob-http-display-async-buffer-immediately t)
  (td-ob-http-default-response 'buffer)
  (td-ob-http-max-time 180))

(use-package ob-python
  :defer t
  :custom
  (org-babel-python-command (or (executable-find "python3.14") "python3")))

;;; Appearance

;; I love eye candy <3. I put quite a lot of efforts to make Emacs look
;; the way I liked.

(setopt inhibit-startup-screen t
        visible-bell nil
        ring-bell-function 'ignore
        scroll-preserve-screen-position t
        scroll-margin 5
        scroll-conservatively 0
        auto-window-vscroll nil)

;; I have a 2k display, and used to run a HiDPI hack for macOS. Recently I got a
;; new Mac and tired of running it. So here it goes:

;; I use mouse scroll a lot, and with the default key binding it would accidentally
;; change the text scale. I don't want this behavior, hence unbind the key here.

(unbind-key "C-<mouse-5>")
(unbind-key "C-<mouse-4>")
(unbind-key "C-<wheel-down>")
(unbind-key "C-<wheel-up>")

;; Default window configuration: half-left of the screen, no scroll bars, no menu
;; bars, no cursor blinking. And btw, nothing beats the classic Monaco. "Menlo",
;; "Source Code Pro" and "Fira Code" come close, currently I have to use them for
;; bold and ligatures support :(.

(setopt
 default-frame-alist
 `((left-fringe . 8) (right-fringe . 4)
   (border-width . 0) (internal-border-width . 0)
   ;;(font . "Iosevka Fixed SS07 16")
   (font . "JetBrains Mono NL 15")
   (tool-bar-lines . 0)
   ;; (fullscreen . maximized)
   (width . 160)
   (height . 50)
   (mac-appearance . dark)
   (ns-appearance . dark)
   (vertical-scroll-bars . nil)))

(blink-cursor-mode -1)
(tool-bar-mode -1)

(setq-default
 cursor-in-non-selected-windows nil
 line-spacing '(0.1 . 0.1)) ;; line-height 1.2, split above/below

(setq ns-use-thin-smoothing t)

(unless (display-graphic-p)
  (menu-bar-mode -1))

(context-menu-mode)

;; Hide unnecessary long mode line mode list

(setopt mode-line-collapse-minor-modes t
        mode-line-front-space " "
        mode-line-end-spaces " ")

(use-package hl-line
  :hook ((prog-mode . hl-line-mode)
         (text-mode . hl-line-mode)))

;; Truncate lines:

(setq-default truncate-lines t)

;; Some preferences that I set for all the theme. Per documentation, the custom
;; theme named =user= will always have the highest priority.

(use-package prism
  :ensure nil
  :load-path "user-lisp"
  :demand t
  :hook (enable-theme-functions . prism-soften-theme-faces))

(use-package pache-dark-theme
  :ensure t
  :config
  (load-theme 'pache-dark t)
  ;; Directly override legacy :bold t attributes that user theme can't always
  ;; neutralize due to the old-style :bold attribute vs modern :weight difference.
  (dolist (face '(font-lock-keyword-face
                  font-lock-function-name-face
                  font-lock-type-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face
                  bold))
    (set-face-attribute face nil :weight 'normal)))

(custom-theme-set-faces
 'user
 '(font-lock-comment-face ((t :slant normal)))
 '(font-lock-comment-delimiter-face ((t :slant normal)))
 '(font-lock-string-face ((t :slant normal)))
 '(font-lock-constant-face ((t :slant normal)))
 '(completions-highlight ((t :inherit region)))

 '(line-number ((t :slant normal :weight normal :foreground "#303634" :background unspecified)))
 '(line-number-current-line ((t :slant normal :weight normal :foreground "#46504D" :background unspecified)))
 '(fringe ((t :inherit line-number :background unspecified)))
  ;; '(vertical-border ((t :foreground "#222")))

 '(hl-line ((t :background "#222")))
 '(show-paren-match ((t :foreground "#f9f5d7" :background "#665C54")))
 '(show-paren-mismatch ((t :foreground "#000000" :background "#FB4934")))

 '(mode-line-buffer-id ((t :foreground "orange")))
 '(cursor ((t :background "orange")))
 '(eglot-highlight-symbol-face ((t :weight normal)))
 '(eglot-code-action-indicator-face ((t :weight normal)))
 '(eglot-inlay-hint-face ((t :height 1.0 :inherit font-lock-comment-face)))
 )

;; Line and column numbers, which I find only helpful when tracking
;; down compiler error :(.

(column-number-mode t)
(line-number-mode t)

(use-package display-line-numbers
  :hook ((prog-mode . display-line-numbers-mode)
           ;(org-mode . display-line-numbers-mode)
         (yaml-mode . display-line-numbers-mode)
         (conf-mode . display-line-numbers-mode))
  :custom
  (display-line-numbers-width-start 100))

;; The default line continuation indicator is too standout and distracting for me.

(define-fringe-bitmap 'halftone
  [#b10100000
   #b01010000]
  nil nil '(top t))

(setcdr (assq 'continuation fringe-indicator-alist) '(nil halftone))
(setcdr (assq 'truncation fringe-indicator-alist) '(nil halftone))

;; Display change marker based on =git=. I usually turn this off because it is
;; kind of distracting, but it is really helpful sometimes.

(defun td/diff-hl-fringe-bmp (_type _pos) 'halftone)

(defun td/diff-hl-overlay-modified (_ov _after-p _beg _end &optional _len)
  "No-op. Markers disappear and reapear is annoying to me.")

(use-package diff-hl
  :ensure t
  :hook (after-init . global-diff-hl-mode)
  :custom
  (diff-hl-draw-borders nil)
  (diff-hl-fringe-bmp-function #'td/diff-hl-fringe-bmp)
  (diff-hl-disable-on-remote t)
  :config
  (custom-theme-set-faces
   'user
   '(diff-hl-insert ((t (:inherit nil :background unspecified :foreground "#81af34"))))
   '(diff-hl-delete ((t (:inherit nil :background unspecified :foreground "#ff0000"))))
   '(diff-hl-change ((t (:inherit nil :background unspecified :foreground "#deae3e")))))

  (advice-add 'diff-hl-overlay-modified :override #'td/diff-hl-overlay-modified))

;;; Misc
(use-package dired
  :custom
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (insert-directory-program "/bin/ls")
  (dired-use-ls-dired nil)
  (dired-listing-switches "-lah")
  (dired-auto-revert-buffer t)
  (dired-kill-when-opening-new-dired-buffer t))

;;; Ideas

;;; init.el ends here
