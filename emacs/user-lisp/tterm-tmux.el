;;; tterm-tmux.el --- Optional tmux launch wrapper -*- lexical-binding: t; -*-
(require 'tterm-process)
(defcustom tterm-tmux-program "tmux"
  "Tmux executable used by the optional Lisp launch wrapper."
  :type 'string :group 'tterm)
(defcustom tterm-tmux-arguments nil
  "Arguments placed before tmux subcommands, such as a private socket name."
  :type '(repeat string) :group 'tterm)
(defcustom tterm-tmux-session-function
  (lambda (_launch) (format "tterm-%s" (substring (md5 (format "%s%s" (float-time) (random))) 0 12)))
  "Function selecting the tmux session name for a launch request.
Return an existing name to attach to that session; return a fresh name
for an independent terminal.  Persistent session selection is Lisp policy."
  :type 'function :group 'tterm)

(defun tterm-tmux-wrap-launch (launch)
  "Wrap LAUNCH in a normal tmux client; no control protocol enters the engine."
  (let ((session (funcall tterm-tmux-session-function launch))
        (command (mapconcat #'shell-quote-argument
                            (cons (plist-get launch :program)
                                  (plist-get launch :args)) " ")))
    (unless (and (stringp session) (string-match-p "\\`[[:alnum:]_-]+\\'" session))
      (user-error "Invalid tmux session name: %s" session))
    (setq launch (copy-sequence launch))
    (plist-put launch :program tterm-tmux-program)
    (plist-put launch :args
               (append tterm-tmux-arguments
                       (list "new-session" "-A" "-s" session "-c" (plist-get launch :cwd) command)))
    launch))

(define-minor-mode tterm-tmux-mode
  "Wrap new tterm launches with tmux using Lisp launch policy.
The native module owns the client PTY and bulk I/O.  Closing a terminal
stops that client; tmux keeps its session.  Disable this mode for direct
shells.  Use `tterm-tmux-session-function' to choose persistent sessions."
  :global t :group 'tterm
  (if tterm-tmux-mode
      (add-hook 'tterm-process-launch-functions #'tterm-tmux-wrap-launch)
    (remove-hook 'tterm-process-launch-functions #'tterm-tmux-wrap-launch)))
(provide 'tterm-tmux)
;;; tterm-tmux.el ends here
