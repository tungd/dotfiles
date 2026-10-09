;;; tterm-process.el --- Lisp launch policy, native process IO -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'subr-x)
(require 'tterm-bridge)

(defcustom tterm-shell-program (or (getenv "SHELL") shell-file-name "/bin/sh")
  "Shell executable started by tterm's native PTY owner."
  :type 'string :group 'tterm)
(defcustom tterm-shell-arguments '("-l")
  "Arguments passed to `tterm-shell-program'."
  :type '(repeat string) :group 'tterm)
(defcustom tterm-process-launch-functions nil
  "Functions transforming a launch plist before native process creation.
Each receives and returns a plist with :program, :args, :environment and
:cwd.  :host identifies the destination; remote shell wrapping happens
AFTER these functions.  These functions may wrap the command with tmux
or another program.  All bulk I/O stays in the native module."
  :type 'hook :group 'tterm)

(defun tterm-process--find-executable (program)
  "Return executable path for PROGRAM using Emacs' runtime environment."
  (or (and (file-name-absolute-p program)
           (file-executable-p program)
           program)
      (executable-find program)
      (let (found)
        (dolist (dir (split-string (or (getenv "PATH") "") path-separator t))
          (let ((candidate (expand-file-name program dir)))
            (when (and (not found) (file-executable-p candidate))
              (setq found candidate))))
        found)))

(defun tterm-process--osc-color (color fallback)
  "Return COLOR as an OSC rgb payload component, falling back to FALLBACK."
  (when-let* ((values (or (and (stringp color)
                               (ignore-errors (color-values color)))
                          (and (stringp fallback)
                               (ignore-errors (color-values fallback))))))
    (format "%04x/%04x/%04x"
            (nth 0 values) (nth 1 values) (nth 2 values))))

(defun tterm-process--default-osc-color (attribute fallback)
  "Return default face ATTRIBUTE as an OSC rgb payload component."
  (let ((color (face-attribute 'default attribute nil 'default)))
    (tterm-process--osc-color color fallback)))

(defun tterm-process--environment ()
  "Return a private environment for a terminal child."
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "TERM" "xterm-256color")
    (setenv "INSIDE_EMACS" (format "%s,tterm" emacs-version))
    (setenv "TTERM_DEFAULT_FOREGROUND"
            (or (tterm-process--default-osc-color :foreground "white") "ffff/ffff/ffff"))
    (setenv "TTERM_DEFAULT_BACKGROUND"
            (or (tterm-process--default-osc-color :background "black") "0000/0000/0000"))
    process-environment))

(defun tterm-process-launch (host cwd &optional program args)
  "Construct and transform a launch request for HOST and CWD."
  (let ((launch (list :program (or program tterm-shell-program)
                      :args (if program args (copy-sequence tterm-shell-arguments))
                      :environment (tterm-process--environment)
                      :host host :cwd cwd)))
    (dolist (transform tterm-process-launch-functions)
      (setq launch (funcall transform launch)))
    (unless (equal host "local")
      (require 'tterm-ssh)
      (setq launch (tterm-ssh-wrap-launch launch)))
    (let* ((default-directory (if (equal host "local") default-directory
                                (expand-file-name "~" "/")))
           (program (plist-get launch :program))
           (executable (and (stringp program)
                            (tterm-process--find-executable program)))
           (directory (plist-get launch :cwd)))
      (unless executable (user-error "Executable not found: %s" program))
      (unless (and (stringp directory) (not (file-remote-p directory))
                   (file-directory-p directory))
        (user-error "Native process needs an existing local directory: %s" directory))
      (plist-put launch :program executable)
      (plist-put launch :cwd (expand-file-name directory))
      launch)))

(provide 'tterm-process)
;;; tterm-process.el ends here
