;;; icloud-mode.el --- Materialize iCloud files before visiting them -*- lexical-binding: t; -*-

(require 'seq)
(require 'subr-x)

(defgroup icloud-mode nil
  "Materialize cloud-backed files before Emacs visits them."
  :group 'files
  :prefix "icloud-mode-")

(defcustom icloud-mode-roots '("~/Documents/")
  "Directories in which dataless files should be materialized.

Entries are expanded before comparison.  Remote files are always ignored."
  :type '(repeat directory)
  :group 'icloud-mode)

(defcustom icloud-mode-download-timeout 60
  "Seconds to wait for a requested cloud file to materialize."
  :type 'number
  :group 'icloud-mode)

(defcustom icloud-mode-poll-interval 0.2
  "Seconds between checks while a cloud file is materializing."
  :type 'number
  :group 'icloud-mode)

(defconst icloud-mode--brctl-program "/usr/bin/brctl"
  "Program used to request an iCloud file download.")

(defconst icloud-mode--stat-program "/usr/bin/stat"
  "Program used to inspect macOS file flags.")

(defun icloud-mode--managed-file-p (file)
  "Return non-nil when FILE is a local file under a configured cloud root."
  (and (eq system-type 'darwin)
       (not (file-remote-p file))
       (file-exists-p file)
       (seq-some
        (lambda (root)
          (file-in-directory-p file (expand-file-name root)))
        icloud-mode-roots)))

(defun icloud-mode--dataless-p (file)
  "Return non-nil when FILE has the macOS dataless flag."
  (with-temp-buffer
    (and (eq 0 (call-process icloud-mode--stat-program nil t nil
                             "-f" "%Sf" file))
         (string-match-p "\\bdataless\\b" (buffer-string)))))

(defun icloud-mode--request-download (file)
  "Ask macOS to download FILE, signaling a `file-error' on failure."
  (with-temp-buffer
    (let ((status (call-process icloud-mode--brctl-program nil t nil
                                "download" file)))
      (unless (eq status 0)
        (signal 'file-error
                (list "Could not request iCloud file download"
                      (string-trim (buffer-string))
                      file))))))

(defun icloud-mode--wait-for-download (file)
  "Wait until FILE is materialized or signal a `file-error'."
  (let ((deadline (+ (float-time) icloud-mode-download-timeout)))
    (while (and (icloud-mode--dataless-p file)
                (< (float-time) deadline))
      (sleep-for icloud-mode-poll-interval))
    (when (icloud-mode--dataless-p file)
      (signal 'file-error
              (list "Timed out downloading iCloud file" file)))))

(defun icloud-mode--materialize (file)
  "Download dataless FILE and wait until its contents are local."
  (message "Downloading iCloud file: %s..." file)
  (icloud-mode--request-download file)
  (icloud-mode--wait-for-download file)
  (message "Downloaded iCloud file: %s" file))

(defun icloud-mode--find-file-noselect (original filename &rest args)
  "Materialize FILENAME before calling ORIGINAL with ARGS."
  (let ((file (expand-file-name filename)))
    (when (and (icloud-mode--managed-file-p file)
               (icloud-mode--dataless-p file))
      (icloud-mode--materialize file))
    (apply original filename args)))

;;;###autoload
(define-minor-mode icloud-mode
  "Materialize dataless iCloud files before visiting them.

This global minor mode advises `find-file-noselect', so it also covers
callers such as Org agenda and capture.  Downloading is synchronous: Emacs
waits until the requested file is local before continuing to visit it."
  :global t
  :group 'icloud-mode
  :lighter nil
  (if icloud-mode
      (advice-add 'find-file-noselect
                  :around #'icloud-mode--find-file-noselect)
    (advice-remove 'find-file-noselect
                   #'icloud-mode--find-file-noselect)))

(provide 'icloud-mode)
;;; icloud-mode.el ends here
