;; -*- lexical-binding: t; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(connection-local-profile-alist
   '((vc-git-connection-default-profile (vc-git--program-version))
     (autogenerated-connection-local-profile/\(:application\ eshell\ :protocol\ \"ssh\"\ :machine\ \"mkt-portal-operator\"\)
      (eshell-path-env-list "/Users/tung/Library/pnpm/"
                            "/Users/tung/Library/Python/3.12/bin/"
                            "/Users/tung/.nix-profile/bin/"
                            "/Users/tung/Projects/dotfiles/bin/"
                            "/Users/tung/.local/sbin/" "/Users/tung/.local/bin/"
                            "/opt/local/bin/" "/opt/local/sbin/"
                            "/usr/local/bin/" "/System/Cryptexes/App/usr/bin/"
                            "/usr/bin/" "/bin/" "/usr/sbin/" "/sbin/"
                            "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin/"
                            "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin/"
                            "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin/"
                            "/opt/X11/bin/" "/Library/Apple/usr/bin/"
                            "/usr/local/MacGPG2/bin/"
                            "/Applications/VMware Fusion.app/Contents/Public/"
                            "/opt/local/bin/" "/opt/local/sbin/"
                            "/Users/tung/.orbstack/bin/"))
     (autogenerated-connection-local-profile/\(:application\ eshell\ :protocol\ \"docker\"\ :machine\ \"oldsystem-game-1\"\)
      (eshell-path-env-list "/Users/tung/Library/pnpm/"
                            "/Users/tung/Library/Python/3.11/bin/"
                            "/Users/tung/.nix-profile/bin/"
                            "/Users/tung/Projects/dotfiles/bin/"
                            "/Users/tung/.local/sbin/" "/Users/tung/.local/bin/"
                            "/opt/local/bin/" "/opt/local/sbin/"
                            "/usr/local/bin/" "/System/Cryptexes/App/usr/bin/"
                            "/usr/bin/" "/bin/" "/usr/sbin/" "/sbin/"
                            "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin/"
                            "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin/"
                            "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin/"
                            "/opt/X11/bin/" "/Library/Apple/usr/bin/"
                            "/usr/local/MacGPG2/bin/"
                            "/Applications/VMware Fusion.app/Contents/Public/"
                            "/opt/local/bin/" "/opt/local/sbin/"
                            "/Users/tung/.orbstack/bin/"))
     (autogenerated-connection-local-profile/\(:application\ eshell\ :protocol\ \"ssh\"\ :machine\ \"clickhouse\"\)
      (eshell-path-env-list "/Users/tung/Library/pnpm/"
                            "/Users/tung/Library/Python/3.11/bin/"
                            "/Users/tung/.nix-profile/bin/"
                            "/Users/tung/Projects/dotfiles/bin/"
                            "/Users/tung/.local/sbin/" "/Users/tung/.local/bin/"
                            "/opt/local/bin/" "/opt/local/sbin/"
                            "/usr/local/bin/" "/System/Cryptexes/App/usr/bin/"
                            "/usr/bin/" "/bin/" "/usr/sbin/" "/sbin/"
                            "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin/"
                            "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin/"
                            "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin/"
                            "/opt/X11/bin/" "/Library/Apple/usr/bin/"
                            "/usr/local/MacGPG2/bin/"
                            "/Applications/VMware Fusion.app/Contents/Public/"
                            "/Users/tung/Library/pnpm/"
                            "/Users/tung/Library/Python/3.11/bin/"
                            "/Users/tung/.nix-profile/bin/"
                            "/Users/tung/Projects/dotfiles/bin/"
                            "/Users/tung/.local/sbin/" "/Users/tung/.local/bin/"
                            "/opt/local/bin/" "/opt/local/sbin/"
                            "/Users/tung/.orbstack/bin/"
                            "/Users/tung/.orbstack/bin/"))
     (autogenerated-connection-local-profile/\(:application\ eshell\ :protocol\ \"kubernetes\"\ :machine\ \"fulong-bd4b54b65-47sj8\"\)
      (eshell-path-env-list "/Users/tung/Library/pnpm/"
                            "/Users/tung/Library/Python/3.11/bin/"
                            "/Users/tung/.nix-profile/bin/"
                            "/Users/tung/Projects/dotfiles/bin/"
                            "/Users/tung/.local/sbin/" "/Users/tung/.local/bin/"
                            "/opt/local/bin/" "/opt/local/sbin/"
                            "/usr/local/bin/" "/System/Cryptexes/App/usr/bin/"
                            "/usr/bin/" "/bin/" "/usr/sbin/" "/sbin/"
                            "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin/"
                            "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin/"
                            "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin/"
                            "/opt/X11/bin/" "/Library/Apple/usr/bin/"
                            "/usr/local/MacGPG2/bin/"
                            "/Users/tung/Library/pnpm/"
                            "/Users/tung/Library/Python/3.11/bin/"
                            "/Users/tung/.nix-profile/bin/"
                            "/Users/tung/Projects/dotfiles/bin/"
                            "/Users/tung/.local/sbin/" "/Users/tung/.local/bin/"
                            "/opt/local/bin/" "/opt/local/sbin/"))
     (remote-detached (detached-shell-program . "/bin/bash")
                      (detached-session-directory . "/tmp/detached"))
     (autogenerated-connection-local-profile/\(:application\ eshell\ :protocol\ \"ssh\"\ :machine\ \"devops\"\)
      (eshell-path-env-list "/Users/tung/.bun/bin" "/Users/tung/Library/pnpm"
                            "/Users/tung/Library/Python/3.11/bin"
                            "/Users/tung/.cargo/bin" "/Users/tung/.yarn/bin"
                            "/Users/tung/.opam/default/bin"
                            "/Users/tung/Projects/dotfiles/bin"
                            "/Users/tung/.local/sbin" "/Users/tung/.local/bin"
                            "/opt/local/bin" "/opt/local/sbin" "/usr/local/bin"
                            "/System/Cryptexes/App/usr/bin" "/usr/bin" "/bin"
                            "/usr/sbin" "/sbin"
                            "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin"
                            "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin"
                            "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin"
                            "/opt/X11/bin" "/Library/Apple/usr/bin"
                            "/usr/local/MacGPG2/bin"))
     (rg-vars-devops (rg-executable . "/bin/rg"))
     (tramp-adb-connection-local-default-ps-profile
      (tramp-process-attributes-ps-args)
      (tramp-process-attributes-ps-format (user . string) (pid . number)
                                          (ppid . number) (vsize . number)
                                          (rss . number) (wchan . string)
                                          (pc . string) (state . string) (args)))
     (tramp-adb-connection-local-default-shell-profile
      (shell-file-name . "/system/bin/sh") (shell-command-switch . "-c"))
     (tramp-flatpak-connection-local-default-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin"
                         "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin"
                         "/local/bin" "/local/freeware/bin" "/local/gnu/bin"
                         "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin"
                         "/opt/bin" "/opt/sbin" "/opt/local/bin"))
     (tramp-kubernetes-connection-local-default-profile
      (tramp-config-check . tramp-kubernetes--current-context-data)
      (tramp-extra-expand-args 97
                               (tramp-kubernetes--container
                                (car tramp-current-connection))
                               104
                               (tramp-kubernetes--pod
                                (car tramp-current-connection))
                               120
                               (tramp-kubernetes--context-namespace
                                (car tramp-current-connection))))
     (tramp-container-connection-local-default-flatpak-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin"
                         "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin"
                         "/local/bin" "/local/freeware/bin" "/local/gnu/bin"
                         "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin"
                         "/opt/bin" "/opt/sbin" "/opt/local/bin"))
     (eshell-connection-default-profile (eshell-path-env-list))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "state=abcde" "-o"
                                        "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number) (euid . number)
                                          (user . string) (egid . number)
                                          (comm . 52) (state . 5)
                                          (ppid . number) (pgrp . number)
                                          (sess . number) (ttname . string)
                                          (tpgid . number) (minflt . number)
                                          (majflt . number)
                                          (time . tramp-ps-time) (pri . number)
                                          (nice . number) (vsize . number)
                                          (rss . number) (etime . tramp-ps-time)
                                          (pcpu . number) (pmem . number) (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o"
                                        "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "stat=abcde" "-o"
                                        "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format (pid . number) (user . string)
                                          (group . string) (comm . 52)
                                          (state . 5) (ppid . number)
                                          (pgrp . number) (ttname . string)
                                          (time . tramp-ps-time) (nice . number)
                                          (etime . tramp-ps-time) (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o"
                                        "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number) (euid . number)
                                          (user . string) (egid . number)
                                          (group . string) (comm . 52)
                                          (state . string) (ppid . number)
                                          (pgrp . number) (sess . number)
                                          (ttname . string) (tpgid . number)
                                          (minflt . number) (majflt . number)
                                          (time . tramp-ps-time) (pri . number)
                                          (nice . number) (vsize . number)
                                          (rss . number) (etime . number)
                                          (pcpu . number) (pmem . number) (args)))
     (tramp-connection-local-default-shell-profile (shell-file-name . "/bin/sh")
                                                   (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile (path-separator . ":")
                                                    (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("5541d412c46ad24030fd34348ce1a5fa5e9030dd335e6d8322aba28fc9ceccc3"
     "5b4fc448d0fb588fdad7c86ab5bb9baaa358488f4b6e4a3d1ff2aca7b293651f"
     "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e"
     "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
     "10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc"
     "bf948e3f55a8cd1f420373410911d0a50be5a04a8886cabe8d8e471ad8fdba8e"
     "adaf421037f4ae6725aa9f5654a2ed49e2cd2765f71e19a7d26a454491b486eb"
     "3fe1ebb870cc8a28e69763dde7b08c0f6b7e71cc310ffc3394622e5df6e4f0da"
     "b99e334a4019a2caa71e1d6445fc346c6f074a05fcbb989800ecbe54474ae1b0"
     "f458b92de1f6cf0bdda6bce23433877e94816c3364b821eb4ea9852112f5d7dc"
     "016f665c0dd5f76f8404124482a0b13a573d17e92ff4eb36a66b409f4d1da410"
     "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948"
     "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b"
     "6c79c891ffc3120b4dfcb8440e808f12d7593a71cbe933c6ecb70290712c5156"
     "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525"
     "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644"
     "7ea883b13485f175d3075c72fceab701b5bf76b2076f024da50dff4107d0db25"
     "4fda8201465755b403a33e385cf0f75eeec31ca8893199266a6aeccb4adedfa4" default))
 '(epg-gpg-program "/usr/local/MacGPG2/bin/gpg2")
 '(package-selected-packages
   '(claudemacs consult crux csv-mode d2-mode detached diff-hl doom-themes dune
                dune-format eat eglot emmet-mode envrc evil exec-path-from-shell
                f feature-mode gptel gptel-anthropic graphql-ts-mode
                highlight-indentation indent-guide isearch-mb kotlin-mode
                kotlin-ts-mode lua-mode magit markdown-ts-mode mermaid-mode
                modus-theme neocaml ob-async ob-http ob-kotlin ob-plantuml
                ob-sql-mode ob-swift pache-dark-theme persp-mode plantuml-mode
                prescient protobuf-mode protobuf-ts-mode reason-mode rg
                scad-mode sql-cassandra surround swift-mode tango-plus-theme
                tempel terraform-mode treesit-fold utop vertico visual-replace
                vterm vundo web-mode window-numbering))
 '(package-vc-selected-packages
   '((claudemacs :url "https://github.com/cpoile/claudemacs.git")
     (claude-code :url "https://github.com/stevemolitor/claude-code.el")
     (neocaml :url "https://github.com/bbatsov/neocaml")))
 '(safe-local-variable-values
   '((checkdoc-allow-quoting-nil-and-t . t)
     (eval setenv "LIBRARY_PATH" "/opt/local/lib/mysql8/mysql")
     (eval add-hook 'after-save-hook
           (lambda nil (org-babel-tangle)
             (byte-recompile-file "~/.config/emacs/init.el"))
           nil t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t :background "orange")))
 '(diff-hl-change ((t (:inherit nil :background unspecified :foreground "#deae3e"))))
 '(diff-hl-delete ((t (:inherit nil :background unspecified :foreground "#ff0000"))))
 '(diff-hl-insert ((t (:inherit nil :background unspecified :foreground "#81af34"))))
 '(eglot-code-action-indicator-face ((t :weight normal)))
 '(eglot-highlight-symbol-face ((t :weight normal)))
 '(eglot-inlay-hint-face ((t :height 1.0)))
 '(font-lock-comment-delimiter-face ((t :slant normal)))
 '(font-lock-comment-face ((t :slant normal)))
 '(font-lock-constant-face ((t :slant normal)))
 '(font-lock-string-face ((t :slant normal)))
 '(fringe ((t :inherit line-number :background unspecified)))
 '(line-number ((t :slant normal)))
 '(line-number-current-line ((t :slant normal)))
 '(mode-line-buffer-id ((t :foreground "#deae3e"))))
