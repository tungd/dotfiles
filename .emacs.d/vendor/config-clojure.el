
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

(provide 'config-clojure)
