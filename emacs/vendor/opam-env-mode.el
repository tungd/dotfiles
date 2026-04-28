;;; opam-env-mode.el --- Auto-activate OPAM env for OCaml projects -*- lexical-binding: t; -*-

(require 'project)
(require 'seq)
(require 'subr-x)

(defgroup opam-env nil
  "Automatically keep Emacs OPAM environment in sync with OCaml projects."
  :group 'tools
  :prefix "opam-env-")

(defcustom opam-env-default-switch "default"
  "Fallback OPAM switch to use when no local switch is detected."
  :type 'string
  :group 'opam-env)

(defcustom opam-env-respect-envrc-opamswitch t
  "When non-nil, do not override OPAMSWITCH if envrc already set it.

This only applies when a project has a .envrc file and OPAMSWITCH is non-empty."
  :type 'boolean
  :group 'opam-env)

(defvar-local opam-env-switch nil
  "Directory-local override for OPAM switch.

When set to a non-empty string, this value takes precedence over auto-detected
local switches and `opam-env-default-switch'.")

(put 'opam-env-switch 'safe-local-variable
     (lambda (value)
       (or (null value)
           (stringp value))))

(defvar opam-env--last-applied nil
  "Cache key for last OPAM environment action.

Stored as one of:
- (:applied ROOT SWITCH)
- (:envrc ROOT OPAMSWITCH)
- nil.

`:applied' is used as the cache key for both successful and failed OPAM
lookups, so repeated hook events do not re-run `opam env' until context changes.")

(defvar opam-env--last-status nil
  "Last status message emitted by opam-env-mode.")

(defvar opam-env--session-switch-overrides (make-hash-table :test #'equal)
  "Session-only OPAM switch overrides keyed by project root.")

(defconst opam-env--root-markers '("dune-project" "_opam" ".opam-switch")
  "Files/directories used to identify likely OCaml project roots.")

(defun opam-env--message-once (format-string &rest args)
  "Log FORMAT-STRING with ARGS only when text changed."
  (let ((msg (apply #'format format-string args)))
    (unless (equal msg opam-env--last-status)
      (setq opam-env--last-status msg)
      (message "%s" msg))))

(defun opam-env--context-directory ()
  "Return current context directory or nil when unavailable."
  (let ((dir (or (and (buffer-file-name)
                      (file-name-directory (buffer-file-name)))
                 default-directory)))
    (when (and dir (not (file-remote-p dir)))
      (file-name-as-directory (expand-file-name dir)))))

(defun opam-env--root-has-opam-file-p (root)
  "Return non-nil when ROOT contains at least one .opam file."
  (when (file-directory-p root)
    (seq-some
     (lambda (name)
       (and (string-match-p "\\.opam\\'" name)
            (file-regular-p (expand-file-name name root))))
     (file-name-all-completions "" root))))

(defun opam-env--ocaml-root-p (root)
  "Return non-nil when ROOT appears to be an OCaml project root."
  (and (file-directory-p root)
       (or (seq-some (lambda (marker)
                       (file-exists-p (expand-file-name marker root)))
                     opam-env--root-markers)
           (opam-env--root-has-opam-file-p root))))

(defun opam-env--project-root-via-project-el (dir)
  "Resolve project root for DIR via project.el.

Returns nil when project.el does not identify a project."
  (let ((default-directory dir))
    (when-let ((project (project-current nil)))
      (file-name-as-directory (expand-file-name (project-root project))))))

(defun opam-env--parent-directory (dir)
  "Return parent directory of DIR, or nil at filesystem root."
  (let ((parent (file-name-directory (directory-file-name dir))))
    (unless (or (null parent) (equal parent dir))
      (file-name-as-directory parent))))

(defun opam-env--locate-ocaml-root (dir)
  "Walk up from DIR to locate a directory that looks like an OCaml root."
  (let ((current (file-name-as-directory (expand-file-name dir)))
        (home (file-name-as-directory (expand-file-name "~")))
        found)
    (while (and current (not found) (not (equal current home)))
      (when (opam-env--ocaml-root-p current)
        (setq found current))
      (unless found
        (setq current (opam-env--parent-directory current))))
    found))

(defun opam-env--project-root ()
  "Return OCaml project root for current context, or nil."
  (when-let ((dir (opam-env--context-directory)))
    (let ((project-root (opam-env--project-root-via-project-el dir)))
      (cond
       ((and project-root (opam-env--ocaml-root-p project-root)) project-root)
       (t (opam-env--locate-ocaml-root dir))))))

(defun opam-env--local-switch-p (root)
  "Return non-nil if ROOT has local OPAM switch metadata."
  (or (file-directory-p (expand-file-name "_opam" root))
      (file-directory-p (expand-file-name ".opam-switch" root))))

(defun opam-env--resolve-switch (root)
  "Resolve OPAM switch for ROOT using configured precedence."
  (let ((override (and (stringp opam-env-switch)
                       (string-trim opam-env-switch)))
        (session-override (gethash root opam-env--session-switch-overrides)))
    (cond
     ((and override (not (string-empty-p override))) override)
     ((and (stringp session-override)
           (not (string-empty-p session-override)))
      session-override)
     ((opam-env--local-switch-p root) root)
     (t opam-env-default-switch))))

(defun opam-env--envrc-opamswitch-p (root)
  "Return non-nil when envrc likely set OPAMSWITCH for ROOT."
  (and opam-env-respect-envrc-opamswitch
       (file-exists-p (expand-file-name ".envrc" root))
       (let ((value (getenv "OPAMSWITCH")))
         (and value (not (string-empty-p value))))))

(defun opam-env--read-opam-env (root switch)
  "Read OPAM env as a sexp list for ROOT and SWITCH.

Returns plist:
- (:ok ENV)
- (:error MESSAGE)"
  (let ((opam (executable-find "opam")))
    (if (not opam)
        '(:error "opam executable not found")
      (with-temp-buffer
        (let ((default-directory root)
              (exit-code (process-file opam nil (current-buffer) nil
                                       "env" "--sexp" "--set-switch"
                                       "--switch" switch)))
          (if (not (eq exit-code 0))
              (list :error (format "opam env failed for switch %s (exit %s)"
                                   switch exit-code))
            (goto-char (point-min))
            (condition-case err
                (list :ok (read (current-buffer)))
              (error
               (list :error (format "failed to parse opam env output: %s"
                                    (error-message-string err)))))))))))

(defun opam-env--current-switch (root)
  "Return the active OPAM switch for ROOT, or nil when unavailable."
  (let ((opam (executable-find "opam")))
    (when opam
      (with-temp-buffer
        (let ((default-directory root)
              (exit-code (process-file opam nil (current-buffer) nil
                                       "switch" "show")))
          (when (eq exit-code 0)
            (let ((switch (string-trim (buffer-string))))
              (unless (string-empty-p switch)
                switch))))))))

(defun opam-env--list-switches (root)
  "Return installed OPAM switches for ROOT."
  (let ((opam (executable-find "opam")))
    (when opam
      (with-temp-buffer
        (let ((default-directory root)
              (exit-code (process-file opam nil (current-buffer) nil
                                       "switch" "list" "--short")))
          (when (eq exit-code 0)
            (seq-filter
             (lambda (switch) (not (string-empty-p switch)))
             (mapcar #'string-trim
                     (split-string (buffer-string) "\n" t)))))))))

(defun opam-env--apply-env (env)
  "Apply OPAM ENV list from `opam env --sexp`."
  (let (path-value)
    (dolist (entry env)
      (when (and (listp entry)
                 (>= (length entry) 2)
                 (stringp (car entry))
                 (stringp (cadr entry)))
        (setenv (car entry) (cadr entry))
        (when (string= (car entry) "PATH")
          (setq path-value (cadr entry)))))
    (when path-value
      (setq exec-path (split-string path-value path-separator t)))))

(defun opam-env--hook-handler (&rest _)
  "Hook dispatcher for `opam-env-mode'."
  (when opam-env-mode
    (opam-env--maybe-activate)))

(defun opam-env--maybe-activate (&optional force)
  "Activate OPAM env for current OCaml project.

When FORCE is non-nil, bypass the cache key check."
  (let ((root (opam-env--project-root)))
    (cond
     ((not root)
      (setq opam-env--last-applied nil))

     ((opam-env--envrc-opamswitch-p root)
      (let ((key (list :envrc root (getenv "OPAMSWITCH"))))
        (unless (and (not force) (equal key opam-env--last-applied))
          (setq opam-env--last-applied key)
          (opam-env--message-once
           "[opam-env] Keeping envrc OPAMSWITCH=%s for %s"
           (getenv "OPAMSWITCH")
           root))))

     (t
     (let* ((switch (opam-env--resolve-switch root))
             (key (list :applied root switch)))
        (unless (and (not force) (equal key opam-env--last-applied))
          (let* ((result (opam-env--read-opam-env root switch))
                 (fallback-switch
                  (and (string= switch opam-env-default-switch)
                       (string= switch "default")
                       (opam-env--current-switch root))))
            (when (and fallback-switch
                       (not (equal fallback-switch switch))
                       (equal (plist-get result :error)
                              (format "opam env failed for switch %s (exit %s)"
                                      switch 2)))
              (setq switch fallback-switch
                    key (list :applied root switch)
                    result (opam-env--read-opam-env root switch)))
            (pcase result
              (`(:ok ,env)
               (opam-env--apply-env env)
               (setq opam-env--last-applied key)
               (opam-env--message-once
                "[opam-env] Activated switch %s for %s"
                switch root))
              (`(:error ,msg)
               (setq opam-env--last-applied key)
               (opam-env--message-once "[opam-env] %s" msg))))))))))

;;;###autoload
(defun opam-env-refresh ()
  "Force refresh OPAM environment for current buffer context."
  (interactive)
  (opam-env--maybe-activate t))

;;;###autoload
(defun opam-env-select-switch (switch)
  "Interactively select SWITCH for the current project and apply its OPAM env."
  (interactive
   (let* ((root (or (opam-env--project-root)
                    (user-error "No OCaml project root found")))
          (switches (or (opam-env--list-switches root)
                        (user-error "Unable to list installed OPAM switches")))
          (current (or (gethash root opam-env--session-switch-overrides)
                       (opam-env--resolve-switch root))))
     (list (completing-read
            (format "OPAM switch for %s: " root)
            switches nil t nil nil current))))
  (let ((root (or (opam-env--project-root)
                  (user-error "No OCaml project root found"))))
    (puthash root switch opam-env--session-switch-overrides)
    (setq opam-env--last-applied nil)
    (opam-env--maybe-activate t)))

;;;###autoload
(defun opam-env-clear-switch-override ()
  "Clear the session switch override for the current project and refresh."
  (interactive)
  (let ((root (or (opam-env--project-root)
                  (user-error "No OCaml project root found"))))
    (remhash root opam-env--session-switch-overrides)
    (setq opam-env--last-applied nil)
    (opam-env--maybe-activate t)))

;;;###autoload
(define-minor-mode opam-env-mode
  "Global minor mode to keep OPAM environment aligned with OCaml projects."
  :global t
  :group 'opam-env
  :lighter " OpamEnv"
  (if opam-env-mode
      (progn
        (setq opam-env--last-applied nil
              opam-env--last-status nil)
        (add-hook 'find-file-hook #'opam-env--hook-handler)
        (add-hook 'dired-mode-hook #'opam-env--hook-handler)
        (add-hook 'buffer-list-update-hook #'opam-env--hook-handler)
        (opam-env--maybe-activate t))
    (remove-hook 'find-file-hook #'opam-env--hook-handler)
    (remove-hook 'dired-mode-hook #'opam-env--hook-handler)
    (remove-hook 'buffer-list-update-hook #'opam-env--hook-handler)
    (setq opam-env--last-applied nil
          opam-env--last-status nil)))

(provide 'opam-env-mode)
;;; opam-env-mode.el ends here
