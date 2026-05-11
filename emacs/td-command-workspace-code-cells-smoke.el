;;; td-command-workspace-code-cells-smoke.el --- code-cells smoke -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'package)

(setq load-prefer-newer t)

(package-initialize)

(unless (require 'code-cells nil t)
  (error "code-cells package is required for td-agent notebook buffers"))

(add-to-list 'load-path
             (file-name-directory (or load-file-name buffer-file-name)))
(require 'td-command-workspace)

(let (run-call)
  (cl-letf (((symbol-function 'td/command-workspace--agent-run)
             (lambda (prompt project-root task-title buffer)
               (setq run-call (list prompt project-root task-title buffer))
               '("/tmp/bin/td-agent" "run"))))
    (with-temp-buffer
      (insert "# td-agent notebook: code-cells smoke\n"
              "# project: /tmp/dotfiles/\n"
              "# task_title: code-cells smoke\n"
              "\n"
              "# %% Draft Notebook Cell\n"
              "# :state: draft\n"
              "\n"
              "Hello from code-cells\n")
      (td/command-workspace-agent-notebook-mode)
      (setq-local td/command-workspace--agent-notebook-project-root
                  "/tmp/dotfiles/")
      (setq-local td/command-workspace--agent-notebook-task-title
                  "code-cells smoke")
      (unless (bound-and-true-p code-cells-mode)
        (error "td-agent notebook mode did not enable code-cells-mode"))
      (unless (eq (lookup-key td/command-workspace-agent-notebook-mode-map
                              (kbd "C-c C-c"))
                  #'td/command-workspace-agent-notebook-submit)
        (error "td-agent notebook mode does not bind C-c C-c to submit"))
      (unless (assq 'td/command-workspace-agent-notebook-mode
                    code-cells-eval-region-commands)
        (error "td-agent notebook mode is missing from code-cells submit commands"))
      (code-cells-eval (point-min) (point-max))
      (unless (equal (nth 0 run-call) "Hello from code-cells")
        (error "code-cells submit passed wrong prompt: %S" run-call))
      (unless (equal (nth 1 run-call) "/tmp/dotfiles/")
        (error "code-cells submit passed wrong project root: %S" run-call))
      (unless (equal (nth 2 run-call) "code-cells smoke")
        (error "code-cells submit passed wrong task title: %S" run-call)))))

(princ "code-cells smoke passed\n")

;;; td-command-workspace-code-cells-smoke.el ends here
