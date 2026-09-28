;;; tterm-bridge.el --- Native module bridge for tterm -*- lexical-binding: t; -*-

;; Loading and thin wrappers for the in-process native terminal module.

(eval-and-compile
  (defconst tterm-bridge--directory
    (file-name-directory (or load-file-name buffer-file-name default-directory))
    "Directory containing tterm bridge Lisp files."))

(require 'url)
(require 'subr-x)

;;; Customization

(defgroup tterm nil
  "Terminal emulator using OCaml engine."
  :group 'applications)

(defcustom tterm-module-path nil
  "Path to tterm-module.so.
If nil, looks in the same directory as tterm.el."
  :type '(choice (const nil) file)
  :group 'tterm)

(defcustom tterm-module-install-directory
  (locate-user-emacs-file "tterm/")
  "Directory where `tterm-install-module' installs prebuilt modules."
  :type 'directory
  :group 'tterm)

(defcustom tterm-module-download-base-url
  "https://github.com/tungd/tterm/releases/latest/download/"
  "Base URL for prebuilt tterm module release assets."
  :type 'string
  :group 'tterm)

(defcustom tterm-tmux-program nil
  "Path to the local tmux executable.
When nil, tterm searches `exec-path' and the PATH entry in
`process-environment'."
  :type '(choice (const nil) file)
  :group 'tterm)

(defcustom tterm-state-file
  (locate-user-emacs-file "tterm/sessions.tsv")
  "Path where tterm persists tmux-backed session hosts.
The environment variable TTERM_STATE_FILE overrides this value."
  :type 'file
  :group 'tterm)

;;; Module loading

(defun tterm-bridge--bundled-module-path ()
  "Return the module path next to `tterm.el'."
  (expand-file-name "tterm-module.so" tterm-bridge--directory))

(defun tterm-bridge--installed-module-path ()
  "Return the user-installed prebuilt module path."
  (expand-file-name "tterm-module.so" tterm-module-install-directory))

(defun tterm-bridge--module-path ()
  "Get the path to tterm-module.so."
  (or tterm-module-path
      (let ((bundled (tterm-bridge--bundled-module-path))
            (installed (tterm-bridge--installed-module-path)))
        (if (file-readable-p bundled)
            bundled
          installed))))

(defun tterm-bridge--module-platform ()
  "Return the current prebuilt module platform suffix."
  (let ((os (pcase system-type
              ('gnu/linux "linux")
              ('darwin "macos")
              (_ nil)))
        (arch (cond
               ((string-match-p "\\(?:x86_64\\|amd64\\)" system-configuration)
                "x86_64")
               ((string-match-p "\\(?:aarch64\\|arm64\\)" system-configuration)
                "arm64")
               (t nil))))
    (unless (and os arch)
      (user-error "No prebuilt tterm module for %s/%s"
                  system-type system-configuration))
    (format "%s-%s" os arch)))

(defun tterm-bridge--module-asset-name ()
  "Return the release asset name for this system."
  (format "tterm-module-%s.%s"
          (tterm-bridge--module-platform)
          (if (eq system-type 'darwin) "dylib" "so")))

(defun tterm-bridge--module-download-url ()
  "Return the release asset URL for this system."
  (concat (file-name-as-directory tterm-module-download-base-url)
          (tterm-bridge--module-asset-name)))

(defun tterm-install-module (&optional overwrite)
  "Download and install the prebuilt tterm module for this system.
With prefix argument OVERWRITE, replace an existing installed module
without prompting."
  (interactive "P")
  (let* ((target (tterm-bridge--installed-module-path))
         (tmp (make-temp-file "tterm-module-" nil ".so"))
         (url (tterm-bridge--module-download-url)))
    (unwind-protect
        (progn
          (when (and (file-exists-p target)
                     (not overwrite)
                     (not (yes-or-no-p
                           (format "Replace existing tterm module at %s? "
                                   target))))
            (user-error "Install cancelled"))
          (make-directory (file-name-directory target) t)
          (url-copy-file url tmp t)
          (rename-file tmp target t)
          (set-file-modes target #o755)
          (message "Installed tterm module from %s to %s" url target)
          target)
      (when (file-exists-p tmp)
        (delete-file tmp)))))

(defun tterm-bridge-ensure-module ()
  "Load the embedded OCaml module if needed."
  (unless (featurep 'tterm-module)
    (let ((module-path (tterm-bridge--module-path)))
      (unless (file-readable-p module-path)
        (user-error "tterm-module.so not found at %s. Run `make' or `M-x tterm-install-module'."
                    module-path))
      (load-file module-path))))

;; These functions are provided by tterm-module.so at runtime.
(declare-function tterm-module--connect "tterm-module" (rows cols host cwd))
(declare-function tterm-module--command "tterm-module" (id command payload))
(declare-function tterm-module--command-start "tterm-module" (id command payload))
(declare-function tterm-module--command-poll "tterm-module" (job))
(declare-function tterm-module--command-cancel "tterm-module" (job))
(declare-function tterm-module--pull-apply-plan-bytes "tterm-module" (id displayed-version))
(declare-function tterm-module--bracketed-paste-enabled "tterm-module" (id))
(declare-function tterm-module--profile-reset "tterm-module" ())
(declare-function tterm-module--profile-enable "tterm-module" (enabled))
(declare-function tterm-module--profile-report "tterm-module" ())

;;; Native wrappers

(defvar tterm-bridge--runtime-configured-p nil
  "Non-nil after the OCaml runtime has received Emacs runtime config.")

(defun tterm-bridge--find-executable (program)
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

(defun tterm-bridge--nonempty-env (name)
  "Return non-empty environment variable NAME, or nil."
  (let ((value (getenv name)))
    (and value (not (string-empty-p value)) value)))

(defun tterm-bridge--legacy-state-file ()
  "Return the pre-Emacs-side default state-file path."
  (expand-file-name
   "tterm/sessions.tsv"
   (or (tterm-bridge--nonempty-env "XDG_STATE_HOME")
       (expand-file-name ".local/state" "~"))))

(defun tterm-bridge--state-file ()
  "Return the effective tterm state-file path."
  (when-let* ((path (or (tterm-bridge--nonempty-env "TTERM_STATE_FILE")
                        tterm-state-file)))
    (expand-file-name path)))

(defun tterm-bridge--osc-color (color fallback)
  "Return COLOR as an OSC rgb payload component, falling back to FALLBACK."
  (when-let* ((values (or (and (stringp color)
                               (ignore-errors (color-values color)))
                          (and (stringp fallback)
                               (ignore-errors (color-values fallback))))))
    (format "%04x/%04x/%04x"
            (nth 0 values) (nth 1 values) (nth 2 values))))

(defun tterm-bridge--default-osc-color (attribute fallback)
  "Return default face ATTRIBUTE as an OSC rgb payload component."
  (let ((color (face-attribute 'default attribute nil 'default)))
    (tterm-bridge--osc-color color fallback)))

(defun tterm-bridge--ensure-state-file (state-file)
  "Ensure STATE-FILE's directory exists and migrate legacy state if needed."
  (when (and state-file (not (string-empty-p state-file)))
    (condition-case nil
        (progn
          (let ((directory (file-name-directory state-file)))
            (when directory
              (make-directory directory t)))
          (unless (tterm-bridge--nonempty-env "TTERM_STATE_FILE")
            (let ((legacy (tterm-bridge--legacy-state-file)))
              (when (and (not (file-exists-p state-file))
                         (file-readable-p legacy))
                (copy-file legacy state-file nil)))))
      (file-error nil))))

(defun tterm-bridge--runtime-config-payload (host)
  "Return OCaml runtime configuration payload for HOST."
  (let* ((tmux-program (tterm-bridge--find-executable
                        (or tterm-tmux-program "tmux")))
         (path (getenv "PATH"))
         (namespace (getenv "TTERM_TMUX_NAMESPACE"))
         (state-file (tterm-bridge--state-file))
         (default-foreground
          (tterm-bridge--default-osc-color :foreground "white"))
         (default-background
          (tterm-bridge--default-osc-color :background "black"))
         (lines
          (delq nil
                (list
                 (and tmux-program
                      (concat "tmux\t" tmux-program))
                 (and path
                      (not (string-empty-p path))
                      (concat "env\tPATH\t" path))
                 (concat "namespace\t"
                         (if (and namespace
                                  (not (string-empty-p namespace)))
                             namespace
                           "default"))
                 (and state-file
                      (not (string-empty-p state-file))
                      (progn
                        (tterm-bridge--ensure-state-file state-file)
                        state-file)
                      (concat "state-file\t" state-file))
                 (and default-foreground
                      (concat "default-color\tforeground\t"
                              default-foreground))
                 (and default-background
                      (concat "default-color\tbackground\t"
                              default-background))))))
    (when (and (or (null host) (string= host "local"))
               (not tmux-program))
      (user-error "tmux executable not found in exec-path or PATH"))
    (mapconcat #'identity lines "\n")))

(defun tterm-bridge-configure-runtime (host &optional force)
  "Configure the OCaml runtime from Emacs before connecting to HOST."
  (when (or force (not tterm-bridge--runtime-configured-p))
    (let ((payload (tterm-bridge--runtime-config-payload host)))
      (when (not (string-empty-p payload))
        (tterm-module--command 0 "configure-runtime" payload)
        (setq tterm-bridge--runtime-configured-p t)))))

(defun tterm-bridge-mark-runtime-config-dirty ()
  "Force the next bridge command to refresh runtime config."
  (setq tterm-bridge--runtime-configured-p nil))

(defun tterm-bridge-connect (rows cols host cwd)
  "Connect to a tmux-backed terminal with ROWS, COLS, HOST, and CWD."
  (tterm-bridge-ensure-module)
  (tterm-bridge-configure-runtime host t)
  (tterm-module--connect rows cols host cwd))

(defun tterm-bridge-command (id command &optional payload)
  "Send COMMAND with PAYLOAD to terminal ID."
  (tterm-bridge-ensure-module)
  (tterm-bridge-configure-runtime
   "local" (member command '("dashboard" "dashboard-local" "dashboard-cached")))
  (tterm-module--command id command (or payload "")))

(defconst tterm-bridge-async-poll-interval 0.04
  "Seconds between async command poll ticks while waiting for a result.
The heavy command runs on the OCaml backend worker thread; the Emacs
main thread only reads the job status on this timer until it completes.")

(defun tterm-bridge-command-async (id command payload callback)
  "Asynchronously run COMMAND with PAYLOAD for terminal ID.
CALLBACK is called exactly once with two arguments: (RESULT ERROR-P).
RESULT is the output string on success, or nil on error/cancellation.
ERROR-P is non-nil (a string describing the failure) when the command
failed or was cancelled.
Returns a job handle (opaque) accepted by `tterm-bridge-command-cancel',
or nil if the command could not be started. Heavy work runs on the OCaml
backend worker thread; this function returns immediately and polls for
completion on a timer."
  (tterm-bridge-ensure-module)
  (condition-case err
      (progn
        (tterm-bridge-configure-runtime
         "local" (member command '("dashboard" "dashboard-local" "dashboard-cached")))
        (let* ((job (tterm-module--command-start id command (or payload "")))
               (state (list :job job :callback callback :timer nil :fired nil))
               (timer (run-at-time
                       0 tterm-bridge-async-poll-interval
                       (lambda ()
                         (unless (plist-get state :fired)
                           (let ((outcome nil)
                                 (outcome-err nil)
                                 (pending t))
                             (condition-case poll-err
                                 (pcase (tterm-module--command-poll job)
                                   (`(done . ,result)
                                    (setq outcome result
                                          pending nil))
                                   (`(error . ,err-msg)
                                    (setq outcome-err err-msg
                                          pending nil))
                                   (`(cancelled . ,_ign)
                                    (setq outcome-err "cancelled"
                                          pending nil))
                                   (`(pending . ,_ign) nil)
                                   (_
                                    (setq outcome-err "unknown async status"
                                          pending nil)))
                               (error
                                (setq outcome-err (error-message-string poll-err)
                                      pending nil)))
                             (unless pending
                               (setf (plist-get state :fired) t)
                               (when (plist-get state :timer)
                                 (cancel-timer (plist-get state :timer)))
                               (funcall callback outcome outcome-err))))))))
          (setf (plist-get state :timer) timer)
          state))
    (error
     (funcall callback nil (error-message-string err))
     nil)))

(defun tterm-bridge-command-cancel (handle)
  "Cancel an in-flight async command started by `tterm-bridge-command-async'.
HANDLE is the job handle returned by that function. Safe to call with nil
or an already-completed job: it stops polling and asks the backend to
cancel, and the original CALLBACK is invoked exactly once with a
cancellation error if it has not already fired."
  (when (and handle (consp handle) (keywordp (car handle)))
    (let ((timer (plist-get handle :timer))
          (job (plist-get handle :job))
          (callback (plist-get handle :callback)))
      (unless (plist-get handle :fired)
        (setf (plist-get handle :fired) t)
        (when timer (cancel-timer timer))
        (ignore-errors (tterm-module--command-cancel job))
        (when callback (funcall callback nil "cancelled"))))))

(defun tterm-bridge-pull-apply-plan-bytes (id displayed-version)
  "Pull serialized apply-plan from terminal ID as #[...] bytecode form."
  (tterm-bridge-ensure-module)
  (tterm-module--pull-apply-plan-bytes id displayed-version))

(defun tterm-bridge-bracketed-paste-enabled-p (id)
  "Return non-nil when terminal ID has enabled bracketed paste."
  (tterm-bridge-ensure-module)
  (tterm-module--bracketed-paste-enabled id))

(defun tterm-bridge-profile-reset ()
  "Reset OCaml pull-diff profiling counters."
  (tterm-bridge-ensure-module)
  (tterm-module--profile-reset))

(defun tterm-bridge-profile-enable (enabled)
  "Enable OCaml pull-diff profiling when ENABLED is non-nil."
  (tterm-bridge-ensure-module)
  (tterm-module--profile-enable enabled))

(defun tterm-bridge-profile-report ()
  "Return OCaml pull-diff profiling counters as a plist string."
  (tterm-bridge-ensure-module)
  (tterm-module--profile-report))

(provide 'tterm-bridge)
;;; tterm-bridge.el ends here
