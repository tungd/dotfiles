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
(declare-function tterm-module--start "tterm-module"
                  (rows cols scrollback program argv environment cwd))
(declare-function tterm-module--write "tterm-module" (id bytes))
(declare-function tterm-module--paste "tterm-module" (id bytes))
(declare-function tterm-module--resize "tterm-module" (id rows cols))
(declare-function tterm-module--close "tterm-module" (id))
(declare-function tterm-module--history "tterm-module" (id offset count))
(declare-function tterm-module--pull "tterm-module" (id displayed-version))
(declare-function tterm-module--profile-reset "tterm-module" ())
(declare-function tterm-module--profile-enable "tterm-module" (enabled))
(declare-function tterm-module--profile-report "tterm-module" ())

;;; Native wrappers

(defun tterm-bridge-start (rows cols scrollback launch)
  "Start LAUNCH through the native process owner."
  (tterm-bridge-ensure-module)
  (let ((program (plist-get launch :program)))
    (tterm-module--start rows cols scrollback program
                         (vconcat (cons program (plist-get launch :args)))
                         (vconcat (plist-get launch :environment))
                         (expand-file-name (plist-get launch :cwd)))))

(defun tterm-bridge-write (id bytes)
  "Queue input BYTES for native terminal ID."
  (tterm-bridge-ensure-module)
  (tterm-module--write id bytes))

(defun tterm-bridge-paste (id bytes)
  "Queue a paste of BYTES for native terminal ID."
  (tterm-bridge-ensure-module)
  (tterm-module--paste id bytes))

(defun tterm-bridge-resize (id rows cols)
  "Resize native terminal ID to ROWS by COLS."
  (tterm-bridge-ensure-module)
  (tterm-module--resize id rows cols))

(defun tterm-bridge-close (id)
  "Close native terminal ID; repeated closes are harmless."
  (tterm-bridge-ensure-module)
  (tterm-module--close id))

(defun tterm-bridge-history (id offset count)
  "Return up to COUNT plain history rows before OFFSET for ID."
  (tterm-bridge-ensure-module)
  (tterm-module--history id offset count))

(defun tterm-bridge-pull (id displayed-version)
  "Pull serialized apply-plan from terminal ID as #[...] bytecode form."
  (tterm-bridge-ensure-module)
  (tterm-module--pull id displayed-version))

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
