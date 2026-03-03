;;; tramp-rpc-python-backend.el --- Python setup advice for tramp-rpc -*- lexical-binding: t; -*-

;; Local glue layer: keep upstream tramp-rpc package untouched,
;; but override deployment to run a vendored Python server script.

(require 'tramp)

;; Silence byte-compiler warnings for functions defined in tramp-sh/tramp-rpc-deploy.
(declare-function tramp-send-command "tramp-sh")
(declare-function tramp-send-command-and-check "tramp-sh")
(declare-function tramp-rpc-deploy--bootstrap-vec "tramp-rpc-deploy")
(declare-function tramp-rpc-deploy--ensure-remote-directory "tramp-rpc-deploy")

(defvar tramp-rpc-deploy-auto-deploy)
(defvar tramp-rpc-deploy-never-deploy)
(defvar tramp-rpc-deploy-remote-directory)

(defgroup td/tramp-rpc-python nil
  "Python backend overrides for tramp-rpc deployment."
  :group 'tramp)

(defcustom td/tramp-rpc-python-command "python3"
  "Python executable on remote hosts used to run tramp-rpc server."
  :type 'string
  :group 'td/tramp-rpc-python)

(defcustom td/tramp-rpc-python-local-script
  (expand-file-name "vendor/tramp-rpc/server/tramp-rpc-server.py" user-emacs-directory)
  "Local path to the vendored TRAMP-RPC Python server script."
  :type 'file
  :group 'td/tramp-rpc-python)

(defcustom td/tramp-rpc-python-remote-script nil
  "Remote script localname.
When nil, defaults to <tramp-rpc-deploy-remote-directory>/tramp-rpc-server-python.py."
  :type '(choice (const :tag "Default under remote directory" nil)
                 (string :tag "Remote script path"))
  :group 'td/tramp-rpc-python)

(defcustom td/tramp-rpc-python-auto-install-msgpack t
  "When non-nil, install `msgpack` with pip on remote hosts if missing."
  :type 'boolean
  :group 'td/tramp-rpc-python)

(defcustom td/tramp-rpc-python-force-server-command nil
  "Optional explicit remote server command.
When non-nil, this command is returned directly."
  :type '(choice (const :tag "Auto command" nil)
                 (string :tag "Remote command"))
  :group 'td/tramp-rpc-python)

(defvar td/tramp-rpc-python--advice-enabled nil
  "Non-nil when Python deployment advices are active.")

(defun td/tramp-rpc-python--remote-script-localname ()
  "Return remote localname for the Python server script."
  (or td/tramp-rpc-python-remote-script
      (concat (file-name-as-directory tramp-rpc-deploy-remote-directory)
              "tramp-rpc-server-python.py")))

(defun td/tramp-rpc-python--server-command ()
  "Return command used to launch the Python server remotely."
  (or td/tramp-rpc-python-force-server-command
      (format "%s %s"
              (tramp-shell-quote-argument td/tramp-rpc-python-command)
              (tramp-shell-quote-argument
               (td/tramp-rpc-python--remote-script-localname)))))

(defun td/tramp-rpc-python--python-command-available-p (vec)
  "Return non-nil if Python command exists on remote VEC."
  (tramp-send-command-and-check
   vec
   (format "command -v %s >/dev/null 2>&1"
           (tramp-shell-quote-argument td/tramp-rpc-python-command))))

(defun td/tramp-rpc-python--msgpack-available-p (vec)
  "Return non-nil if Python msgpack is importable on remote VEC."
  (tramp-send-command-and-check
   vec
   (format "%s -c %s >/dev/null 2>&1"
           (tramp-shell-quote-argument td/tramp-rpc-python-command)
           (tramp-shell-quote-argument "import msgpack"))))

(defun td/tramp-rpc-python--install-msgpack (vec)
  "Install Python msgpack via pip on remote VEC."
  (tramp-send-command-and-check
   vec
   (format
    "%s -m pip install --user msgpack >/dev/null 2>&1 || %s -m pip install --user --break-system-packages msgpack >/dev/null 2>&1"
    (tramp-shell-quote-argument td/tramp-rpc-python-command)
    (tramp-shell-quote-argument td/tramp-rpc-python-command))))

(defun td/tramp-rpc-python--ensure-msgpack (vec)
  "Ensure Python msgpack exists on remote VEC."
  (unless (td/tramp-rpc-python--msgpack-available-p vec)
    (if (not td/tramp-rpc-python-auto-install-msgpack)
        (signal 'remote-file-error
                (list "Python module 'msgpack' not available on remote host"
                      (tramp-file-name-host vec)))
      (message "Installing python msgpack on %s..." (tramp-file-name-host vec))
      (unless (td/tramp-rpc-python--install-msgpack vec)
        (signal 'remote-file-error
                (list "Failed to install python msgpack via pip on remote host"
                      (tramp-file-name-host vec))))
      (unless (td/tramp-rpc-python--msgpack-available-p vec)
        (signal 'remote-file-error
                (list "Python msgpack still unavailable after install attempt"
                      (tramp-file-name-host vec)))))))

(defun td/tramp-rpc-python--remote-script-exists-p (vec)
  "Return non-nil when Python server script exists on remote VEC."
  (tramp-send-command-and-check
   vec
   (format "test -x %s"
           (tramp-shell-quote-argument
            (td/tramp-rpc-python--remote-script-localname)))))

(defun td/tramp-rpc-python--copy-script (vec)
  "Copy vendored Python server script to remote VEC and chmod +x it."
  (unless (file-exists-p td/tramp-rpc-python-local-script)
    (signal 'remote-file-error
            (list "Python server script not found" td/tramp-rpc-python-local-script)))
  (let ((remote-path (tramp-make-tramp-file-name
                      vec
                      (td/tramp-rpc-python--remote-script-localname))))
    (tramp-rpc-deploy--ensure-remote-directory vec)
    (copy-file td/tramp-rpc-python-local-script remote-path t)
    (tramp-send-command
     vec
     (format "chmod +x %s"
             (tramp-shell-quote-argument
              (td/tramp-rpc-python--remote-script-localname))))))

(defun td/tramp-rpc-python-deploy-ensure-binary (vec)
  "Advice replacement for `tramp-rpc-deploy-ensure-binary` using Python backend."
  (let ((bootstrap-vec (tramp-rpc-deploy--bootstrap-vec vec)))
    (unless (td/tramp-rpc-python--python-command-available-p bootstrap-vec)
      (signal 'remote-file-error
              (list "Python command not found on remote host"
                    td/tramp-rpc-python-command
                    (tramp-file-name-host vec))))

    (td/tramp-rpc-python--ensure-msgpack bootstrap-vec)

    (if tramp-rpc-deploy-never-deploy
        (unless (td/tramp-rpc-python--remote-script-exists-p bootstrap-vec)
          (signal 'remote-file-error
                  (list
                   "Python server script not found on remote host (never-deploy is set)"
                   (tramp-file-name-host vec))))
      (unless (td/tramp-rpc-python--remote-script-exists-p bootstrap-vec)
        (if tramp-rpc-deploy-auto-deploy
            (progn
              (message "Deploying tramp-rpc python server to %s..."
                       (tramp-file-name-host vec))
              (td/tramp-rpc-python--copy-script bootstrap-vec))
          (signal 'remote-file-error
                  (list "Python server script not found on remote host and auto-deploy is disabled"
                        (tramp-file-name-host vec))))))

    (td/tramp-rpc-python--server-command)))

(defun td/tramp-rpc-python-deploy-expected-binary-localname ()
  "Advice replacement for `tramp-rpc-deploy-expected-binary-localname`."
  (td/tramp-rpc-python--server-command))

(defun td/tramp-rpc-python-enable ()
  "Enable Python deployment advices for tramp-rpc."
  (interactive)
  (require 'tramp-rpc-deploy)
  (unless td/tramp-rpc-python--advice-enabled
    (advice-add 'tramp-rpc-deploy-ensure-binary :override
                #'td/tramp-rpc-python-deploy-ensure-binary)
    (advice-add 'tramp-rpc-deploy-expected-binary-localname :override
                #'td/tramp-rpc-python-deploy-expected-binary-localname)
    (setq td/tramp-rpc-python--advice-enabled t)))

(defun td/tramp-rpc-python-disable ()
  "Disable Python deployment advices for tramp-rpc."
  (interactive)
  (when td/tramp-rpc-python--advice-enabled
    (advice-remove 'tramp-rpc-deploy-ensure-binary
                   #'td/tramp-rpc-python-deploy-ensure-binary)
    (advice-remove 'tramp-rpc-deploy-expected-binary-localname
                   #'td/tramp-rpc-python-deploy-expected-binary-localname)
    (setq td/tramp-rpc-python--advice-enabled nil)))

(provide 'tramp-rpc-python-backend)
;;; tramp-rpc-python-backend.el ends here
