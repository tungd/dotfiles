;;; tterm-profiling.el --- Profiling helpers for tterm -*- lexical-binding: t; -*-

;; Profiling support for the OCaml terminal engine and Emacs apply-plan apply path.

(eval-and-compile
  (defconst tterm-profiling--directory
    (file-name-directory (or load-file-name buffer-file-name default-directory))
    "Directory containing tterm profiling Lisp files."))

(require 'cl-lib)
(require 'tterm-bridge (expand-file-name "tterm-bridge" tterm-profiling--directory))

(defvar tterm--apply-profile-enabled nil
  "Whether Emacs-side apply-plan apply profiling is enabled.")

(defvar tterm--apply-profile nil
  "Hash table holding Emacs-side apply-plan apply profiling counters.")

(defun tterm--profile-reset ()
  "Reset OCaml pull-diff profiling counters."
  (tterm-bridge-profile-reset))

(defun tterm--profile-enable (enabled)
  "Enable OCaml pull-diff profiling when ENABLED is non-nil."
  (tterm-bridge-profile-enable enabled))

(defun tterm--profile-report ()
  "Return OCaml pull-diff profiling counters as a plist string."
  (tterm-bridge-profile-report))

(defun tterm--apply-profile-reset ()
  "Reset Emacs-side apply-plan apply profiling counters."
  (setq tterm--apply-profile (make-hash-table :test 'eq)))

(defun tterm--apply-profile-enable (enabled)
  "Enable Emacs-side apply-plan apply profiling when ENABLED is non-nil."
  (setq tterm--apply-profile-enabled enabled))

(defun tterm--apply-profile-inc (key &optional amount)
  "Increment apply profile counter KEY by AMOUNT."
  (when tterm--apply-profile-enabled
    (unless tterm--apply-profile
      (tterm--apply-profile-reset))
    (puthash key
             (+ (gethash key tterm--apply-profile 0)
                (or amount 1))
             tterm--apply-profile)))

(defun tterm--apply-profile-add-time (key seconds)
  "Add SECONDS to apply profile timer KEY."
  (when tterm--apply-profile-enabled
    (unless tterm--apply-profile
      (tterm--apply-profile-reset))
    (puthash key
             (+ (gethash key tterm--apply-profile 0.0) seconds)
             tterm--apply-profile)))

(defmacro tterm--apply-profile-time (key &rest body)
  "Run BODY and accumulate elapsed time under apply profile KEY."
  (declare (indent 1))
  (let ((start-var (make-symbol "tterm--apply-profile-start")))
    `(if tterm--apply-profile-enabled
         (let ((,start-var (float-time)))
           (prog1 (progn ,@body)
             (tterm--apply-profile-add-time
              ,key (- (float-time) ,start-var))))
       ,@body)))

(defun tterm--apply-profile-report ()
  "Return Emacs-side apply-plan apply profiling counters as a plist string."
  (let ((profile tterm--apply-profile)
        (keys '(:calls :ops
                :plain-ops :plain-rows :plain-ms
                :styled-ops :styled-rows :styled-ms
                :scroll-ops :scroll-ms
                :cursor-ops :cursor-ms :cursor-sync-ms
                :delete-row-ops :delete-row-ms
                :insert-row-ops :insert-row-ms
                :link-ops :link-spans :link-ms
                :title-ops :title-ms
                :osc-ops :osc-ms
                :full-row-run-ops :full-row-runs :full-row-lines :full-row-ms
                :partial-spans :partial-span-ms
                :text-decode-ms
                :row-navigation-calls :row-navigation-ms
                :link-property-ms)))
    (prin1-to-string
     (apply #'append
            (mapcar
             (lambda (key)
               (list key
                     (if profile
                         (let ((value (gethash key profile 0)))
                           (if (string-suffix-p "-ms" (symbol-name key))
                               (* 1000.0 value)
                             value))
                       0)))
             keys)))))

(provide 'tterm-profiling)
;;; tterm-profiling.el ends here
