;;; tterm-ssh.el --- Optional SSH launch wrapper -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'subr-x)

(defun tterm-ssh-wrap-launch (launch)
  "Wrap remote LAUNCH in a native-owned SSH process."
  (let* ((command (mapconcat #'shell-quote-argument
                            (cons (plist-get launch :program)
                                  (plist-get launch :args)) " "))
         (cwd (plist-get launch :cwd))
         (directory (if (or (null cwd) (equal cwd "") (equal cwd "~"))
                        "$HOME" (shell-quote-argument cwd)))
         (environment (plist-get launch :environment))
         (remote-environment
          (cl-remove-if-not
           (lambda (entry) (string-match-p
                            "\\`\\(?:TERM\\|INSIDE_EMACS\\|TTERM_DEFAULT_FOREGROUND\\|TTERM_DEFAULT_BACKGROUND\\)=" entry))
           environment)))
    (list :program "ssh"
          :args (list "-tt" "--" (plist-get launch :host)
                      (format "cd %s && exec env %s %s"
                              directory
                              (mapconcat #'shell-quote-argument remote-environment " ")
                              command))
          :environment environment
          :cwd (expand-file-name "~" "/"))))

(provide 'tterm-ssh)
;;; tterm-ssh.el ends here
