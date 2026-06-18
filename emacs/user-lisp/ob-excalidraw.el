;;; ob-excalidraw.el --- Org-Babel support for Excalidraw -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Excalidraw Export Contributors
;; Keywords: literate programming, reproducible research, drawing
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/tungd/excalidraw-cli

;;; Commentary:

;; Org-Babel support for Excalidraw diagrams.
;;
;; This package integrates Excalidraw into org-babel, allowing you to:
;; - Embed .excalidraw diagrams inline in org files
;; - Render them to SVG on C-c C-c
;; - Edit them visually via Excalidraw on C-c '
;;
;; Usage:
;;   (require 'ob-excalidraw)
;;   (ob-excalidraw-setup)
;;   (add-to-list 'org-babel-load-languages '(excalidraw . t))
;;
;; Source block format:
;;
;;   #+begin_src excalidraw :file diagram.svg :scale 2
;;     {
;;       "type": "excalidraw",
;;       "version": 2,
;;       "elements": [...]
;;     }
;;   #+end_src
;;
;; Header arguments:
;;   :file    - Output file path (SVG). Required.
;;   :scale   - Output scale factor (default 1.0)
;;
;; Requires `excalidraw-cli' to be on your PATH.

;;; Code:

(require 'ob)

;;; Customization

(defgroup ob-excalidraw nil
  "Org-babel integration for Excalidraw."
  :group 'org-babel
  :prefix "ob-excalidraw-")

(defcustom ob-excalidraw-export-executable "excalidraw-cli"
  "Path to the excalidraw-cli executable."
  :type 'string
  :group 'ob-excalidraw)

(defcustom ob-excalidraw-serve-executable nil
  "Path to the excalidraw-cli executable for serve mode.
Falls back to `ob-excalidraw-export-executable' when nil."
  :type '(choice (const nil) string)
  :group 'ob-excalidraw)

(defcustom ob-excalidraw-auto-open-browser t
  "If non-nil, automatically open the browser when launching the editor."
  :type 'boolean
  :group 'ob-excalidraw)

(defcustom ob-excalidraw-editor-theme "light"
  "Theme for the Excalidraw editor.
Either \"light\" or \"dark\"."
  :type '(choice (const "light") (const "dark"))
  :group 'ob-excalidraw)

;;; Default header arguments

(defvar org-babel-default-header-args:excalidraw
  '((:results . "file")
    (:exports . "results"))
  "Default arguments for evaluating an Excalidraw source block.")

;;; Internal helpers

(defun ob-excalidraw--serve-executable ()
  "Return the executable path for serve mode."
  (or ob-excalidraw-serve-executable ob-excalidraw-export-executable))

(defun ob-excalidraw--write-body-to-temp (body)
  "Write BODY (excalidraw JSON) to a temp .excalidraw file.
Returns the file path."
  (let ((temp-file (make-temp-file "ob-excalidraw-" nil ".excalidraw")))
    (with-temp-file temp-file
      (insert body))
    temp-file))

;;; org-babel functions

(defun org-babel-execute:excalidraw (body params)
  "Execute a block of Excalidraw code with org-babel.
BODY is the content of the source block (excalidraw JSON).
PARAMS is a property list of source block parameters."
  (let* ((out-file (or (cdr (assq :file params))
                       (error "You need to specify a :file parameter")))
         (scale (cdr (assq :scale params)))
         (in-file (ob-excalidraw--write-body-to-temp body))
         (scale-arg (if scale (format "--scale %g" scale) ""))
         (cmd (format "%s %s -o %s %s"
                      (shell-quote-argument ob-excalidraw-export-executable)
                      (shell-quote-argument in-file)
                      (shell-quote-argument out-file)
                      scale-arg)))
    (message "excalidraw-cli: rendering %s" out-file)
    (let ((exit-code (org-babel-eval cmd "")))
      (delete-file in-file)
      (when (and exit-code (not (zerop exit-code)))
        (error "excalidraw-cli failed with exit code %d" exit-code)))
    nil))

(defun org-babel-prep-session:excalidraw (_session _params)
  "Return an error because Excalidraw does not support sessions."
  (error "Excalidraw does not support sessions"))

;;; Edit-special integration

(defun ob-excalidraw--edit-source-block (edit-fn &rest args)
  "Advice around `org-edit-src-code' to intercept excalidraw blocks.
When the block language is `excalidraw', launch the visual editor
instead of opening a text buffer.  Otherwise call EDIT-FN with ARGS."
  (let ((info (org-babel-get-src-block-info t)))
    (if (and info (string= (nth 0 (cdr info)) "excalidraw"))
        (ob-excalidraw-launch-editor)
      (apply edit-fn args))))

(defun ob-excalidraw-launch-editor ()
  "Launch the Excalidraw visual editor for the block at point.
This writes the block body to a temp file, starts the Excalidraw
local server, and opens the browser for visual editing."
  (interactive)
  (let* ((info (org-babel-get-src-block-info 'light))
         (_ (unless info (user-error "No excalidraw source block at point")))
         (lang (nth 0 (cdr info)))
         (_ (unless (string= lang "excalidraw")
              (user-error "Not an excalidraw source block")))
         (body (nth 1 info))
         (params (nth 2 info))
         (out-file (cdr (assq :file params)))
         (excalidraw-file
          (if out-file
              (concat (file-name-sans-extension out-file) ".excalidraw")
            (make-temp-file "excalidraw-edit-" nil ".excalidraw")))
         (serve-cmd
          (format "%s serve %s --output %s --theme %s"
                  (shell-quote-argument (ob-excalidraw--serve-executable))
                  (shell-quote-argument excalidraw-file)
                  (if out-file
                      (shell-quote-argument out-file)
                    "--stdout")
                  (shell-quote-argument ob-excalidraw-editor-theme))))
    (with-temp-file excalidraw-file
      (insert body))
    (message "Opening Excalidraw editor for %s ..." excalidraw-file)
    (let ((proc (start-process-shell-command
                 "excalidraw-editor" nil serve-cmd)))
      (set-process-sentinel
       proc
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (message "Excalidraw editor closed. Re-rendering...")
           (ob-excalidraw--refresh-block-at-point
            excalidraw-file out-file params)))))))

(defun ob-excalidraw--refresh-block-at-point (excalidraw-file out-file params)
  "Refresh the org-babel source block after editing.
EXCALIDRAW-FILE is the .excalidraw file that was edited.
OUT-FILE is the output file path.  PARAMS are the block params."
  (let ((info (org-babel-get-src-block-info 'light)))
    (when info
      (let ((body (when (file-exists-p excalidraw-file)
                    (with-temp-buffer
                      (insert-file-contents excalidraw-file)
                      (buffer-string)))))
        (when body
          ;; Update the source block body in the org buffer
          (save-excursion
            (org-babel-goto-src-block)
            (when (re-search-forward
                   "^[ \t]*#\\+begin_src[^\n]*\n\\([^\0]*?\\)[\n]*[ \t]*#\\+end_src"
                   nil t)
              (let ((body-beg (match-beginning 1))
                    (body-end (match-end 1)))
                (delete-region body-beg body-end)
                (goto-char body-beg)
                (insert body)))))
        ;; Re-render if we have an output file
        (when out-file
          (let ((scale (cdr (assq :scale params))))
            (ob-excalidraw--render excalidraw-file out-file scale)
            (message "Re-rendered %s" out-file)))))))

(defun ob-excalidraw--render (in-file out-file &optional scale)
  "Render IN-FILE to OUT-FILE.  Optional SCALE factor."
  (let ((scale-arg (if scale (format "--scale %g" scale) ""))
        (cmd (format "%s %s -o %s %s"
                     (shell-quote-argument ob-excalidraw-export-executable)
                     (shell-quote-argument in-file)
                     (shell-quote-argument out-file)
                     scale-arg)))
    (org-babel-eval cmd "")))

;;; Advice installation

;;;###autoload
(defun ob-excalidraw-setup ()
  "Set up ob-excalidraw by advising org-edit-src-code.
This intercepts C-c ' for excalidraw blocks and launches the
visual editor instead."
  (interactive)
  (advice-add 'org-edit-src-code :around #'ob-excalidraw--edit-source-block)
  (message "ob-excalidraw: edit-special advice installed."))

;;;###autoload
(defun ob-excalidraw-teardown ()
  "Remove the ob-excalidraw advice from org-edit-src-code."
  (interactive)
  (advice-remove 'org-edit-src-code #'ob-excalidraw--edit-source-block)
  (message "ob-excalidraw: edit-special advice removed."))

(provide 'ob-excalidraw)

;;; ob-excalidraw.el ends here
