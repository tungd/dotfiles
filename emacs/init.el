;;; init.el --- Tung Dao's Emacs Setup -*- lexical-binding: t; -*-


;; So, as an effort to improve the responsiveness, I'm going to reboot my Emacs
;; configuration. Hope it is better this time.

;; The idea is that I'm going to add things in, bit by bit, just enough to get it
;; going. This way I will be able to nail down the packages that cause issue, and
;; look for alternatives.

;;; Enable Lexical Binding

;; Make things a little bit faster. For context: https://www.emacswiki.org/emacs/DynamicBindingVsLexicalBinding

(setenv
 "LIBRARY_PATH"
 (string-join
  '("/opt/local/lib/gcc14"
    "/opt/local/lib/gcc14/gcc/aarch64-apple-darwin24/14.2.0"
    "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib")
  ":"))

;;; Packages and initialization

;; All the packages I used are from MELPA (https://melpa.org). However, I install them automatically
;; with =use-package.el= instead of using =package.el= directly. Since Emacs 29.1,
;; =use-package.el= has been bundled with Emacs.

;; Also, Emacs 29.1 added the =package-vc-install= command, which is really handy as
;; quite a few of the packages I used are not available on MELPA yet.

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  :custom
  (package-quickstart t))

(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(use-package bind-key
  :functions (override-global-mode bind-key--remove)
  :hook (after-init . override-global-mode))

;; Sometimes I write my own package, or download package from Emacs wiki; they
;; are stored in the =~/.emacs.d/vendor= directory.

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "vendor/"))
(add-to-list 'load-path (concat user-emacs-directory "vendor/"))
(add-to-list 'exec-path "/opt/local/bin")
(add-to-list 'exec-path "~/Library/Python/3.12/bin/")
(add-to-list 'exec-path "~/Library/pnpm")
(add-to-list 'exec-path "~/.local/bin")
(add-to-list 'exec-path "~/.opam/default/bin/")
(add-to-list 'exec-path "~/.claude/local/")
(setenv "PATH" (string-join exec-path ":"))

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

(use-package midnight
  :hook (after-init . midnight-mode)
  :config
  (add-hook 'midnight-hook #'td/trash-desktop-screenshots)
  (add-hook 'midnight-hook #'td/trash-stale-node-modules))

;;;; Scratch Buffer

;; Make the scratch buffer persistent and use org-mode by default.

(defconst td/scratch-file
  (expand-file-name "scratch.org" user-emacs-directory)
  "File where scratch buffer content is persisted.")

(defun td/scratch-restore ()
  "Restore scratch buffer content from file."
  (when (file-exists-p td/scratch-file)
    (with-current-buffer "*scratch*"
      (erase-buffer)
      (insert-file-contents td/scratch-file)
      (set-buffer-modified-p nil))))

(defun td/scratch-save ()
  "Save scratch buffer content to file."
  (with-current-buffer "*scratch*"
    (write-region (point-min) (point-max) td/scratch-file nil 'quietly)
    (set-buffer-modified-p nil)))

(setopt initial-scratch-message nil
        initial-major-mode 'org-mode)

(add-hook 'after-init-hook #'td/scratch-restore)
(add-hook 'kill-emacs-hook #'td/scratch-save)
(add-hook 'save-buffers-kill-terminal-hook #'td/scratch-save)

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

(defun td/trash-desktop-screenshots ()
  "Move screenshot files from Desktop to trash."
  (interactive)
  (let ((desktop-dir (expand-file-name "~/Desktop/")))
    (when (file-directory-p desktop-dir)
      (dolist (file (directory-files desktop-dir t "^Screenshot.*\\.png$"))
        (when (file-regular-p file)
          (move-file-to-trash file)
          (message "Trashed: %s" file))))))

(defun td/trash-stale-node-modules ()
  "Trash node_modules folders whose parent hasn't been touched in 7 days.
Expects structure: ~/Projects/<org>/<project>/node_modules"
  (interactive)
  (let ((projects-dir (expand-file-name "~/Projects/"))
        (stale-days 7))
    (when (file-directory-p projects-dir)
      (dolist (org-dir (directory-files projects-dir t "^[^.]"))
        (when (file-directory-p org-dir)
          (dolist (project-dir (directory-files org-dir t "^[^.]"))
            (when (file-directory-p project-dir)
              (let ((node-modules (expand-file-name "node_modules" project-dir)))
                (when (and (file-directory-p node-modules)
                           (> (time-to-seconds
                               (time-subtract (current-time)
                                              (file-attribute-modification-time
                                               (file-attributes project-dir))))
                              (* stale-days 24 60 60)))
                  (move-file-to-trash node-modules)
                  (message "Trashed: %s" node-modules))))))))))

;;; Navigation

;;;; File position
(use-package saveplace
  :hook (after-init . save-place-mode))

;;;; Projects
(use-package files
  :custom
  (remote-file-name-access-timeout 5)
  (find-file-visit-truename t))

;; Emacs built-in =project.el= has gone a long way so I'm using that now. There are
;; still some missing features coming from =projectile=, but I can live with that.

(use-package project
  :commands (project-find-file project-vc-dir project-current)
  :custom
  (project-file-history-behavior 'relativize)
  :config
  (autoload 'magit-project-status "magit-extras" nil t)
  (keymap-set project-prefix-map "m" #'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))

(use-package rg
  :ensure t
  :bind ("C-c s" . rg-menu))

;;;; Symbols

;; Using =dumb-jump= with =xref= integration. Fast, no server needed, works well
;; with Claude for deeper code understanding.

;; Default key bindings is

;; | Binding   | Command                              | Description |
;; |-----------+--------------------------------------+-------------|
;; | =M-.=     | =xref-find-definitions=              |             |
;; | =C-M-p=   | =xref-find-definitions-other-window= |             |
;; | =C-M-g=   | =xref-pop-marker-stack=              |             |

(use-package grep
  :custom
  (grep-command "rg -nS --no-heading ")
  (grep-use-null-device nil))

(use-package xref
  :bind (("C-M-p" . xref-find-definitions-other-window)
         ("C-M-g" . xref-go-back))
  :hook ((xref-after-update . outline-minor-mode))
  :custom
  (xref-search-program 'ripgrep))

(use-package dumb-jump
  :ensure t
  :init (add-hook 'xref-backend-functions 'dumb-jump-xref-activate))

;;;; Mini-buffer

;; Shortcut key to go back to the home directory, works regardless of the
;; mini-buffer completion framework.

(use-package minibuffer
  :hook (minibuffer-setup . td/setup-minibuffer-auto-complete)
  :custom
  (minibuffer-visible-completions t)
  (completion-auto-help 'always)
  (completion-show-help nil)
    ;(completion-auto-select 'second-tab)
    ;(completion-auto-select nil)
  (completion-auto-select t)
  (completions-max-height 20)
  (completions-sort 'historical)
  (completions-format 'one-column)
  (completions-detailed t)
  (completions-group t))

(bind-key "TAB" #'minibuffer-complete minibuffer-mode-map)

(bind-key "C-p" #'minibuffer-previous-completion minibuffer-local-map)
(bind-key "C-n" #'minibuffer-next-completion minibuffer-local-map)

(bind-key "C-p" #'minibuffer-previous-completion completion-in-region-mode-map)
(bind-key "C-n" #'minibuffer-next-completion completion-in-region-mode-map)

(ido-mode -1)

(defun td/minibuffer-auto-complete ()
  "Show completions if minibuffer has 2+ characters."
  (when (and (minibufferp)
             (>= (length (minibuffer-contents)) 2)
             (memq this-command '(self-insert-command
                                  delete-backward-char
                                  backward-delete-char-untabify)))
    (minibuffer-completion-help)))

(defun td/setup-minibuffer-auto-complete ()
  "Set up auto-completion for this minibuffer session."
  (add-hook 'post-command-hook #'td/minibuffer-auto-complete nil t))

(use-package prescient
  :ensure t
  :hook (after-init . prescient-persist-mode)
  :init
  (add-to-list 'completion-styles 'prescient)
  (setq completion-preview-sort-function #'prescient-completion-sort))

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
  :config
  (add-to-list 'recentf-exclude "elpa/.*")
  (add-to-list 'recentf-exclude "__init__.py")
  (add-to-list 'recentf-exclude "_build/*")
  (add-to-list 'recentf-exclude "node_modules/.*"))

(bind-key* "C-;" #'execute-extended-command)
(global-set-key (kbd "C-l") ctl-x-map)

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
(bind-key* [remap keyboard-quit] #'crux-keyboard-quit-dwim)
(bind-key* [remap kill-line] #'crux-smart-kill-line)
(bind-key* "s-n" #'next-buffer)
(bind-key* "s-p" #'previous-buffer)

;; Make the file executable if starting with "shebang":

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

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
  (auto-revert-check-vc-info t))

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

;;;; Snippets

;; I've since switched to =Tempel= instead of =Yasnippet=. With Copilot, the
;; suggestions is my snippet/template. Coupled with Eglot/LSP for
;; function/method-based templates, I rarely need a library of
;; snippets/templates. For the occasional needs that is specific to me/my workflow,
;; a more minimal template library like =Tempo=/=Tempel= is suffice.

;; I settled with =Tempel=, it polished some of the rough edges with =Tempo=, namely:

;; - Per-language/major-mode templates. =Tempo= does support this in the form of
;; tags, however it requires some glue code, while =Tempel= has built-in support
;; - Temporary key map for moving between placeholders/poi/marks

;; Since the template definition is compatible between the 2, I can easily move to
;; =Tempo= in the future if it added support for the 2 points above.

(use-package tempel
  :ensure t
  :hook (after-init . global-tempel-abbrev-mode)
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert)))

;; Tempo integration code for future reference:

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

(use-package conflict-buttons
  :ensure t
  :defer t
  :hook (smerge-mode . conflict-buttons-mode))

;;; Shell and remote
(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))

(use-package opam-env-mode
  :ensure nil
  :hook (after-init . opam-env-mode))

(use-package comint
  :bind ("C-c C-l" . comint-clear-buffer)
  :custom
  (comint-terminfo-terminal "dumb-emacs-ansi"))

(use-package vterm
  :ensure t
  :custom
  (vterm-shell "/bin/zsh -l"))

(use-package detached
  :ensure t
  :hook (after-init . detached-init)
  :bind (;; Replace `async-shell-command' with `detached-shell-command'
         ([remap async-shell-command] . detached-shell-command)
           ;; Replace `compile' with `detached-compile'
           ;; ([remap compile] . detached-compile)
           ;; ([remap recompile] . detached-recompile)
           ;; Replace `project-compile' with `detached-project-compile'
           ;; ([remap project-compile] . detached-project-compile)
           ;; Replace built in completion of sessions with `consult'
         ([remap detached-open-session] . detached-consult-session))
  :custom ((detached-show-output-on-attach t)
           (detached-terminal-data-command system-type)
           (detached-notification-function #'td/detached-macos-notification))
  :config
  (defun td/detached-macos-notification (session)
      "Send macOS native notification when SESSION completes."
      (let* ((status (detached-session-status session))
             (host (detached-session-host-name session))
             (command (detached-session-command session))
             (title (if (eq status 'success)
                        (format "Detached finished [%s]" host)
                      (format "Detached failed [%s]" host))))
        (ns-do-applescript
         (format "display notification %S with title %S" command title)))))

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

(use-package msgpack
  :ensure t)

(use-package tramp-rpc
  :vc (:url "https://github.com/ArthurHeymans/emacs-tramp-rpc"
       :branch "master"
       :rev :newest)
  :after tramp
  :init
  (require 'tramp-rpc-python-backend)
  :custom
  (td/tramp-rpc-python-command "python3")
  (td/tramp-rpc-python-local-script
   (expand-file-name "vendor/tramp-rpc/server/tramp-rpc-server.py" user-emacs-directory))
    ;; Keep nil by default; set via connection-local vars for host-specific commands.
  (td/tramp-rpc-python-force-server-command nil)
  :config
  (td/tramp-rpc-python-enable))

;; Some speedup for Tramp:

(use-package vc
  :custom
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git))
  (vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp)))

;;; Programming

;; Native LSP support via =Eglot= since Emacs 29.1 (disabled - using dumb-jump + Claude instead)

;; I'm experimenting with working purely with LLM and without LSP. It's not that I don't like LSP, it's just that I don't think it's efficient, especially in the era of LLM.

(use-package swift-mode
  :ensure t)

;; Native Tree-sitter support since Emacs 29

;; The swift one is a bit tricky. The =parser.c= and =grammar.json= files are auto-generated, we need to download them from the CI build (https://github.com/alex-pinkus/tree-sitter-swift), then copy them to the source folder. After that we can use the following function call to build it:

;; TODO: automating the entire process

(defun td/treesit-mark-node (node)
  (goto-char (treesit-node-start node))
  (call-interactively #'set-mark-command)
  (goto-char (treesit-node-end node)))

(defun td/tressit-expand-region ()
  "Poor man's expand-region, worked surprisingly well for me"
  (interactive)
  (if (treesit-language-at (point))
      (let ((start (if (region-active-p) (region-beginning) 1))
            (end (if (region-active-p) (region-end) 1))
            (node (if (region-active-p)
                      (treesit-node-parent
                       (treesit-node-on (region-beginning) (region-end)))
                    (treesit-node-at (point)))))
        (if (or (/= start (treesit-node-start node))
                (/= end (treesit-node-end node)))
            (td/treesit-mark-node node)
          (forward-char)
          (td/tressit-expand-region)))
    (mark-sexp 1 t)))

(bind-key "M--" #'td/tressit-expand-region)

(use-package treesit
  :functions (treesit-node-on
              treesit-node-at
              treesit-node-parent
              treesit-node-start
              treesit-node-end
              treesit-node-prev-sibling)
  :config
  (add-to-list 'treesit-language-source-alist '(kotlin . ("https://github.com/fwcd/tree-sitter-kotlin.git")))
  (add-to-list 'treesit-language-source-alist '(protobuf . ("https://github.com/casouri/tree-sitter-module.git")))
  (add-to-list 'treesit-language-source-alist '(swift . ("https://github.com/alex-pinkus/tree-sitter-swift.git"))))

(defun td/treesit-indent-debug (n p _bol)
  (message
   "treesit-indent-debug: %s %s %s"
   n p (treesit-node-prev-sibling n)))

(defun td/treesit-tag-start (_n p _bol)
  (save-excursion
    (goto-char (treesit-node-start p))
    (search-forward "<")
    (- (point) 1)))

(defun td/treesit-tag-sibling (n p bol)
  (when treesit--indent-verbose
    (td/treesit-indent-debug n p bol))
  (let* ((tag (treesit-parent-until
               p
               (rx (or "jsx_closing_element" "jsx_element" "jsx_self_closing_element"))))
         (prev (treesit-node-prev-sibling tag)))
    (when treesit--indent-verbose
      (message "tag: %s, prev: %s" tag prev))
    (cond
     ((treesit-node-match-p prev (rx "jsx_opening_element"))
        ;; This is the first child, need to check the parent tag
      (let ((parent-tag (treesit-parent-until tag "jsx_element")))
        (+ (td/treesit-tag-start tag parent-tag 0) typescript-ts-mode-indent-offset)))
     ((treesit-node-match-p tag (rx "jsx_closing_element"))
      (let ((parent-tag (treesit-parent-until tag "jsx_element")))
        (td/treesit-tag-start tag parent-tag 0)))
     (t (save-excursion
          (goto-char (treesit-node-start prev))
          (while (and (<= (point) (point-max))
                      (looking-at (rx (| whitespace control)) t))
            (forward-char))
          (point))))))

(defvar td/tsx-additional-indent-rules
  '(((match nil "<") td/treesit-tag-sibling 0)
    ((parent-is "jsx_text") parent-bol 2)
    ((node-is "jsx_closing_element") td/treesit-tag-start 0)
    ((match "/" "jsx_self_closing_element") td/treesit-tag-start 0)
    ((match ">" "jsx_opening_element") td/treesit-tag-start 0)
    ((parent-is "jsx_opening_element") td/treesit-tag-start 2)
    ((parent-is "jsx_self_closing_element") td/treesit-tag-start 2)))

(defun td/fix-tsx-indentation ()
  (setq-local
   treesit-simple-indent-rules
   (list (cons 'tsx (append td/tsx-additional-indent-rules (cdar (typescript-ts-mode--indent-rules 'tsx)))))))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
    ;:hook ((tsx-ts-mode . td/fix-tsx-indentation))
  )

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

;; TODO: continue using my GLM subscription for now. Switch back to Gemini when the subscription is over <2026-04-02 Thu>

(use-package gptel
  :ensure t
  :bind ("C-l a" . gptel-menu)
  :hook (gptel-mode . visual-line-mode)
  :config
  (require 'gptel-org)
  (require 'gptel-zai)
  (gptel-zai-setup)
  (setopt
  gptel-default-mode 'org-mode
  gptel-model 'glm-4.7))

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
  (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options))

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

(use-package neocaml
  :ensure t
  :defer t
  :custom
  (neocaml-repl-program-name "dune")
  (neocaml-repl-program-args '("utop" "." "--" "-short-paths")))

(use-package utop
  :ensure t
  :custom
  (utop-command "dune utop . -- -emacs"))

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

;; - A default =inbox.org= on Desktop for tasks capturing and project management
;; - Nicer display with inline images
;; - Enable GTD todo keyword sequence and time logging

(use-package ob-plantuml
  :custom
  (org-plantuml-jar-path "/opt/local/share/java/plantuml/plantuml.jar"))

(defun td/org-electric-pair ()
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
  (org-default-notes-file (expand-file-name "inbox.org" org-directory))
  (org-agenda-files `(,org-directory))
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
     (plantuml . t)
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
         ("C-c o t" . org-todo-list))
  :custom
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-window-setup 'current-window)
  (org-stuck-projects
   '("+project+LEVEL=1/-WAITING-DONE-CANCELED" ("TODO" "WAITING") nil "")))

;;;; Note taking

;; As stated earlier, I practice GTD. Working projects and new stuffs go to
;; =inbox.org= file. Old tasks are archived to =archive.org=. Here's my
;; =org-capture= templates to dump stuffs to =inbox/note=

(use-package org-capture
  :bind* (("C-c o c" . org-capture))
  :custom
  (org-capture-templates
   `(("t" "Inbox item" entry
      (file+headline "~/Desktop/inbox.org" "Inbox") nil)
     ("l" "TIL" entry
      (file+olp+datetree "~/Desktop/inbox.org" "TIL") nil
      :jump-to-captured t)
     ("b" "Blog" entry
      (file+olp+datetree "~/Desktop/inbox.org" "Blog") nil
      :jump-to-captured t))))

;;;; Literate programming

;; Org Babel for literate programming and API documentation.

(use-package ob-core
  :defer t
  :hook (org-babel-after-execute . org-display-inline-images)
  :custom
  (org-confirm-babel-evaluate nil))

(use-package ob-http
  :defer t
  :ensure t
  :custom
  (ob-http:max-time 180)
  (ob-http:remove-cr t))

(use-package ob-python
  :defer t
  :custom
  (org-babel-python-command "/opt/local/bin/python3.14"))

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
     ;; (font . "Monaco 16")
     ;; (font . "Iosevka Fixed SS07 16")
     ;; (font . "Iosevka Mono 16")
   (font . "JetBrains Mono NL 16")
     ;; (font . "Menlo 15")
     ;; (font . "Google Sans Code 16")
     ;; (font . "Fira Mono 16")
     ;; (font . "Ubuntu Mono 16")
   (tool-bar-lines . 0)
   (fullscreen . maximized)
   (mac-appearance . dark)
   (ns-appearance . dark)
   (vertical-scroll-bars . nil)))

(blink-cursor-mode -1)
(tool-bar-mode -1)

(setq-default
 cursor-in-non-selected-windows nil
 line-spacing nil
 )

(setq ns-use-thin-smoothing t)

(unless (display-graphic-p)
  (menu-bar-mode -1))

(context-menu-mode)

;; Hide unnecessary long mode line mode list

(setopt mode-line-collapse-minor-modes t)

(use-package hl-line
  :hook ((prog-mode . hl-line-mode)
         (text-mode . hl-line-mode)))

;; Truncate lines:

(setq-default truncate-lines t)

;; Some preferences that I set for all the theme. Per documentation, the custom
;; theme named =user= will always have the highest priority.

(setq modus-themes-common-palette-overrides
      '(;; Make the main background a soft dark gray instead of pure black
          ;; (bg-main "#1e1e1e")
          ;; Make the "dim" background (used for line numbers/sidebar) slightly lighter
          ;; (bg-dim  "#282828")
          ;; Optional: Soften the white text so it's not "piercing"
        (fg-main "#cfcfcf"))
      modus-themes-italic-constructs t
      modus-themes-bold-constructs nil    ;; Bold text can sometimes "bleed" on 4K screens
      modus-themes-mixed-fonts nil          ;; Use variable pitch for docs/org-mode
      modus-themes-variable-pitch-ui nil)

(load-theme 'modus-vivendi t)

(custom-theme-set-faces
 'user
 '(font-lock-comment-face ((t :slant normal)))
 '(font-lock-comment-delimiter-face ((t :slant normal)))
 '(font-lock-string-face ((t :slant normal)))
 '(font-lock-constant-face ((t :slant normal)))

 '(line-number ((t :slant normal :foreground unspecified :inherit font-lock-comment-face)))
 '(line-number ((t :slant normal :background unspecified :foreground "#444" :inherit font-lock-comment-face)))
 '(line-number-current-line ((t :slant normal :weight normal :foreground "#fff")))
 '(fringe ((t :inherit line-number :background unspecified)))
 '(vertical-border ((t :foreground "#222")))

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
  (dired-listing-switches "-lah")
  (dired-auto-revert-buffer t)
  (dired-kill-when-opening-new-dired-buffer t))

(defun td/refresh-front-most-tab ()
  (interactive)
  (shell-command "osascript -e 'tell application \"Firefox\" to reload active tab of window 1'"))

(bind-key* "C-M-r" #'td/refresh-front-most-tab)

(use-package eat
  :ensure t)

(setq use-default-font-for-symbols nil)
(set-fontset-font t 'symbol "Menlo" nil 'prepend)
(set-fontset-font t 'emoji "Menlo" nil 'prepend)

;;; Ideas

;;;; Emacs cron job to watch github build of any of the opening project
(use-package notmuch
  :ensure t
  :defer t
  :config
  (setq notmuch-archive-tags '("-inbox" "-unread"))
    ;; 1. Point to the MacPorts binary explicitly (avoids PATH issues)
  (setq notmuch-command "~/.local/bin/notmuch")

    ;; 2. Performance: asynchronous search to avoid blocking Emacs
  (setq notmuch-search-oldest-first nil)

    ;; 3. Theme Integration: notmuch usually works great with modus-vivendi-tinted,
    ;;    but you can force specific faces if needed.
    ;;    (setq notmuch-search-line-faces ...) ;; Usually not needed with Modus.

    ;; Load custom extensions
  (load (expand-file-name "vendor/notmuch-custom.el" user-emacs-directory) t))

;;; init.el ends here
