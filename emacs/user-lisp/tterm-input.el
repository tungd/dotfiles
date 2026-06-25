;;; tterm-input.el --- Terminal input encoding and dispatch -*- lexical-binding: t; -*-

;;; Commentary:
;; Key-to-terminal-byte encoding plus input dispatch commands used by tterm.

;;; Code:

(require 'cl-lib)
(require 'dnd)
(require 'subr-x)
(require 'tramp nil t)
(require 'url-util)
(require 'yank-media nil t)

(defcustom tterm-file-paste-directory
  (expand-file-name "tterm-paste/" temporary-file-directory)
  "Directory used for clipboard media files pasted into tterm."
  :type 'directory
  :group 'tterm)

(defcustom tterm-remote-file-send-directory "/tmp/"
  "Remote directory used by `tterm-send-file' before pasting paths."
  :type 'directory
  :group 'tterm)

(defvar tterm-redraw-update-delay)
(defvar tterm--redraw-request-timer)
(defvar tterm--terminal)

(declare-function tterm-application-cursor "tterm" (term))
(declare-function tterm-host "tterm" (term))
(declare-function tterm-id "tterm" (term))
(declare-function tterm--local-file-from-uri "tterm-osc" (uri))
(declare-function tterm--redraw-active-p "tterm-mode" ())
(declare-function tterm--redraw-now-unless-resizing "tterm-mode" ())
(declare-function tterm--paste-input "tterm" (id bytes))
(declare-function tterm--write-input "tterm" (id bytes))

(defconst tterm--special-key-codes
  '((up . "\e[A")
    (down . "\e[B")
    (right . "\e[C")
    (left . "\e[D")
    (home . "\e[H")
    (end . "\e[F")
    (insert . "\e[2~")
    (delete . "\e[3~")
    (prior . "\e[5~")
    (next . "\e[6~")
    (f1 . "\eOP")
    (f2 . "\eOQ")
    (f3 . "\eOR")
    (f4 . "\eOS")
    (f5 . "\e[15~")
    (f6 . "\e[17~")
    (f7 . "\e[18~")
    (f8 . "\e[19~")
    (f9 . "\e[20~")
    (f10 . "\e[21~")
    (f11 . "\e[23~")
    (f12 . "\e[24~"))
  "Special key names mapped to terminal byte sequences.")

(defconst tterm--application-cursor-key-codes
  '((up . "\eOA")
    (down . "\eOB")
    (right . "\eOC")
    (left . "\eOD"))
  "Arrow key byte sequences used when DECCKM is enabled.")

(defun tterm--input-bytes (key)
  "Return terminal bytes for KEY."
  (tterm--input-bytes-for-mode key nil))

(defun tterm--input-bytes-for-mode (key application-cursor)
  "Return terminal bytes for KEY, honoring APPLICATION-CURSOR mode."
  (cond
   ((stringp key) (encode-coding-string key 'utf-8))
   ((and (consp key) (eq (car key) 'meta))
    (concat (string ?\e)
            (tterm--input-bytes-for-mode (cdr key) application-cursor)))
   ((symbolp key)
    (or (and application-cursor
             (cdr (assq key tterm--application-cursor-key-codes)))
        (cdr (assq key tterm--special-key-codes))
        (user-error "Unsupported special key: %S" key)))
   (t (user-error "Unsupported key input: %S" key))))

;;; Input dispatch

(defun tterm--schedule-input-redraw ()
  "Schedule immediate redraw after terminal input."
  (when (and (not noninteractive)
             (eq major-mode 'tterm-mode)
             (tterm--redraw-active-p))
    (when (timerp tterm--redraw-request-timer)
      (cancel-timer tterm--redraw-request-timer))
    (let ((buffer (current-buffer)))
      (cl-labels
          ((schedule
            (delay follow-up)
            (setq tterm--redraw-request-timer
                  (run-at-time
                   delay nil
                   (lambda ()
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (setq tterm--redraw-request-timer nil)
                         (when (and tterm--terminal
                                    (tterm--redraw-active-p))
                           (tterm--redraw-now-unless-resizing)
                           (when follow-up
                             (schedule tterm-redraw-update-delay nil))))))))))
        (schedule 0 t)))))

(defun tterm--send-key (key)
  "Send KEY to the terminal."
  (let ((term tterm--terminal))
    (when term
      (let* ((id (tterm-id term))
             (bytes (tterm--input-bytes-for-mode
                     key
                     (tterm-application-cursor term))))
        (tterm--write-input id bytes)
        (tterm--schedule-input-redraw)))))

(defun tterm--self-insert (n)
  "Insert N self-inserting characters into the terminal."
  (interactive "p")
  (ignore n)
  (tterm--send-key (this-command-keys)))

(defun tterm--newline (n)
  "Send newline to terminal."
  (interactive "p")
  (ignore n)
  (tterm--send-key "\r"))

(defun tterm--backspace (n)
  "Send backspace to terminal."
  (interactive "p")
  (ignore n)
  (tterm--send-key "\C-?"))

(defun tterm--delete (n)
  "Send forward delete to terminal."
  (interactive "p")
  (ignore n)
  (tterm--send-key 'delete))

(defun tterm--backward-kill-word (&optional n)
  "Send terminal backward-kill-word."
  (interactive "p")
  (ignore n)
  (tterm--send-key "\C-w"))

(defun tterm--tab (n)
  "Send tab to terminal."
  (interactive "p")
  (ignore n)
  (tterm--send-key "\C-i"))

(defun tterm--escape (n)
  "Send escape to terminal."
  (interactive "p")
  (ignore n)
  (tterm--send-key "\e"))

(defun tterm--control-key (key)
  "Send control key KEY to terminal."
  (unless (and (stringp key) (= 1 (length key)))
    (user-error "Expected a one-character key string"))
  (let ((code (string-to-char key)))
    (unless (characterp code)
      (user-error "Expected a one-character key"))
    (tterm--send-key (string (logand #x1f (downcase code))))))

(defun tterm-send-eof ()
  "Send Ctrl-D/EOT to the terminal."
  (interactive)
  (tterm--control-key "D"))

(defun tterm-send-interrupt ()
  "Send Ctrl-C/ETX to the terminal."
  (interactive)
  (tterm--control-key "C"))

(defun tterm--meta-key (key)
  "Send Meta KEY to terminal as ESC-prefixed input."
  (tterm--send-key (cons 'meta key)))

(defun tterm--paste (&optional text)
  "Send text to terminal. TEXT defaults to latest kill entry."
  (interactive)
  (unless (and (null text)
               (tterm--paste-clipboard-media))
    (let* ((payload (or text (current-kill 0 t)))
           (term tterm--terminal))
      (when term
        (tterm--paste-input (tterm-id term) payload)
        (tterm--schedule-input-redraw)))))

(defun tterm--paste-file-text (paths)
  "Return terminal input text for local file PATHS."
  (mapconcat #'shell-quote-argument paths " "))

(defun tterm--remote-host ()
  "Return the remote host for the current tterm buffer, or nil for local."
  (when-let* ((term tterm--terminal)
              (host (tterm-host term)))
    (unless (or (string-empty-p host)
                (string= host "local"))
      host)))

(defun tterm--remote-file-name (host path)
  "Return the TRAMP file name for HOST and remote local PATH."
  (concat "/ssh:" host ":" path))

(defun tterm--remote-send-directory ()
  "Return normalized remote directory used for sent files."
  (let ((directory (file-name-as-directory tterm-remote-file-send-directory)))
    (unless (file-name-absolute-p directory)
      (user-error "Remote file send directory must be absolute: %s" directory))
    directory))

(defun tterm--remote-send-path (source)
  "Return a unique remote-local /tmp path for SOURCE."
  (let* ((basename (file-name-nondirectory (directory-file-name source)))
         (basename (if (string-empty-p basename) "file" basename))
         (token (file-name-nondirectory
                 (make-temp-name "tterm-send-file-"))))
    (expand-file-name
     (concat token "-" basename)
     (tterm--remote-send-directory))))

(defun tterm--terminal-file-path (path)
  "Return the file path visible inside the current terminal for PATH.
For a local terminal this is PATH.  For a remote terminal, copy PATH to
`tterm-remote-file-send-directory' on the remote host and return that
remote-local path."
  (let ((source (expand-file-name path)))
    (unless (and (stringp source) (file-readable-p source))
      (user-error "File is not readable: %s" source))
    (if-let* ((host (tterm--remote-host)))
        (let* ((remote-directory (tterm--remote-send-directory))
               (remote-path (tterm--remote-send-path source))
               (tramp-directory (tterm--remote-file-name host remote-directory))
               (tramp-path (tterm--remote-file-name host remote-path)))
          (unless (featurep 'tramp)
            (user-error "TRAMP is required to send files to remote tterm buffers"))
          (make-directory tramp-directory t)
          (copy-file source tramp-path t)
          remote-path)
      source)))

(defun tterm--send-files (paths)
  "Send file PATHS to the terminal as shell-quoted pane-visible paths."
  (let ((paths (delq nil paths)))
    (unless paths
      (user-error "No files selected"))
    (tterm--paste
     (tterm--paste-file-text
      (mapcar #'tterm--terminal-file-path paths)))))

(defun tterm--paste-files (paths)
  "Paste file PATHS into the terminal as shell-quoted pane-visible paths."
  (tterm--send-files paths))

(defun tterm-send-file (path)
  "Read a file with minibuffer completion and send it to the terminal.
Remote tterm buffers upload the file to the remote host before pasting
the remote-visible path."
  (interactive
   (list (read-file-name "Send file: " default-directory nil t)))
  (tterm--send-files (list (expand-file-name path))))

(defalias 'tterm-paste-file #'tterm-send-file)

(defun tterm--media-file-extension (mime-type)
  "Return a filename extension for MIME-TYPE."
  (let ((name (symbol-name mime-type)))
    (cond
     ((equal name "image/jpeg") "jpg")
     ((equal name "image/svg+xml") "svg")
     ((string-match "\\`image/\\([[:alnum:]+.-]+\\)\\'" name)
      (match-string 1 name))
     (t "bin"))))

(defun tterm--write-media-file (mime-type data)
  "Write clipboard media DATA for MIME-TYPE and return its path."
  (make-directory tterm-file-paste-directory t)
  (let* ((extension (tterm--media-file-extension mime-type))
         (path (make-temp-file "tterm-paste-" nil
                               (concat "." extension)
                               tterm-file-paste-directory))
         (coding-system-for-write 'binary))
    (write-region data nil path nil 'silent)
    path))

(defun tterm--yank-media-file (mime-type data)
  "Save clipboard media DATA for MIME-TYPE and paste the file path."
  (unless (and (stringp data) (> (length data) 0))
    (user-error "Clipboard media is empty"))
  (tterm--paste-files (list (tterm--write-media-file mime-type data))))

(defun tterm--paste-clipboard-media (&optional noselect)
  "Paste supported clipboard media as a temporary file path, if present."
  (and (fboundp 'yank-media)
       (condition-case nil
           (progn
             (condition-case err
                 (if noselect
                     (yank-media noselect)
                   (yank-media))
               (wrong-number-of-arguments
                (if noselect
                    (yank-media)
                  (signal (car err) (cdr err)))))
             t)
         (wrong-number-of-arguments nil)
         (user-error nil))))

(defun tterm-paste-clipboard-media (&optional noselect)
  "Paste supported clipboard media into tterm as a temporary file path.
With prefix NOSELECT, ask `yank-media' which clipboard type to use."
  (interactive "P")
  (unless (tterm--paste-clipboard-media noselect)
    (when (called-interactively-p 'interactive)
      (user-error "No supported clipboard media for tterm"))))

(defun tterm--dnd-event-urls (event)
  "Return URL strings carried by drag-and-drop EVENT."
  (let (urls)
    (cl-labels ((collect (value)
                  (cond
                   ((stringp value)
                    (push value urls))
                   ((and (consp value)
                         (not (windowp (car value))))
                    (dolist (item value)
                      (collect item))))))
      (dolist (item (cddr event))
        (collect item)))
    (nreverse urls)))

(defun tterm--dnd-local-file (url)
  "Return a readable local file path from dropped URL, or nil."
  (when (stringp url)
    (cond
     ((string-prefix-p "file:" url)
      (or (dnd-get-local-file-name url t)
          (let ((path (tterm--local-file-from-uri url)))
            (and path (file-readable-p path) path))))
     ((file-readable-p url)
      (expand-file-name url)))))

(defun tterm--image-file-p (path)
  "Return non-nil when PATH names a readable image file."
  (and (stringp path)
       (file-readable-p path)
       (ignore-errors (image-type-from-file-header path))))

(defun tterm--file-url (path)
  "Return encoded file URL for local PATH."
  (let ((segments (split-string (expand-file-name path) "/" t)))
    (concat "file:///" (mapconcat #'url-hexify-string segments "/"))))

(defun tterm--paste-image-file-urls (paths)
  "Paste local image PATHS as file URLs for image-aware terminal apps."
  (dolist (path paths)
    (tterm--paste (tterm--file-url path))))

(defun tterm--dnd-send-files (event)
  "Send dropped local file paths from EVENT to the terminal."
  (interactive "e")
  (let ((paths (delq nil (mapcar #'tterm--dnd-local-file
                                  (tterm--dnd-event-urls event)))))
    (if paths
        (progn
          (if (and (not (tterm--remote-host))
                   (cl-every #'tterm--image-file-p paths))
              (tterm--paste-image-file-urls paths)
            (tterm--send-files paths))
          'private)
      (user-error "No local readable files in drop"))))

(defun tterm--send-special-key (name)
  "Send special key bytes for NAME."
  (interactive)
  (tterm--send-key name))

(defun tterm--control-key-command (key)
  "Return an interactive command that sends control KEY to the terminal."
  (lambda ()
    (interactive)
    (tterm--control-key key)))

(provide 'tterm-input)

;;; tterm-input.el ends here
