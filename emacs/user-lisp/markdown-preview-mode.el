;;; markdown-preview-mode.el --- Markdown preview helpers -*- lexical-binding: t; -*-

(require 'subr-x)

(defgroup markdown-preview nil
  "Preview Markdown files and Mermaid blocks."
  :group 'markdown)

(defconst markdown-preview-mermaid-fence-open-regexp
  "^[[:blank:]]*\\(```+\\|~~~+\\)[[:blank:]]*\\(?:mermaid\\|mmd\\)\\b.*$")

(defconst markdown-preview-fence-close-regexp
  "^[[:blank:]]*\\(```+\\|~~~+\\)[[:blank:]]*$")

(defcustom markdown-preview-chrome-executable-candidates
  '("/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
    "/Applications/Chromium.app/Contents/MacOS/Chromium"
    "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"
    "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge")
  "Chrome-family executables to try for Mermaid CLI."
  :type '(repeat file)
  :group 'markdown-preview)

(defcustom markdown-preview-renderer
  (expand-file-name "~/Projects/dotfiles/bin/mdrender")
  "CLI used to render whole Markdown files."
  :type 'file
  :group 'markdown-preview)

(defun markdown-preview--chrome-executable ()
  "Return a local Chrome-family executable for Mermaid CLI."
  (catch 'found
    (dolist (path markdown-preview-chrome-executable-candidates)
      (when (file-executable-p path)
        (throw 'found path)))))

(defun markdown-preview--mermaid-block-bounds ()
  "Return bounds of the Mermaid fence around point."
  (let ((origin (point))
        open-start
        beg
        end
        close-end)
    (save-excursion
      (when (or (looking-at markdown-preview-mermaid-fence-open-regexp)
                (re-search-backward markdown-preview-mermaid-fence-open-regexp nil t))
        (setq open-start (match-beginning 0))
        (forward-line 1)
        (setq beg (point))
        (when (re-search-forward markdown-preview-fence-close-regexp nil t)
          (setq end (match-beginning 0)
                close-end (match-end 0))
          (when (and (<= open-start origin) (<= origin close-end))
            (cons beg end)))))))

;;;###autoload
(defun markdown-preview-mermaid-block (&optional browse)
  "Render Mermaid fence at point to SVG.
With prefix argument BROWSE, open the SVG in the browser."
  (interactive "P")
  (let ((mmdc (or (executable-find "mmdc")
                  (user-error "Install Mermaid CLI: pnpm add -g @mermaid-js/mermaid-cli")))
        (bounds (markdown-preview--mermaid-block-bounds)))
    (unless bounds
      (user-error "Point is not in a Mermaid fenced code block"))
    (let* ((base (make-temp-file "emacs-mermaid-"))
           (input (concat base ".mmd"))
           (output (concat base ".svg"))
           (buffer (get-buffer-create "*mermaid*")))
      (write-region (car bounds) (cdr bounds) input nil 'silent)
      (with-current-buffer buffer
        (erase-buffer))
      (if (let ((process-environment (copy-sequence process-environment)))
            (when-let* ((chrome (markdown-preview--chrome-executable)))
              (push (concat "PUPPETEER_EXECUTABLE_PATH=" chrome)
                    process-environment))
            (let ((status (call-process mmdc nil buffer nil
                                        "-i" input "-o" output "-b" "transparent")))
              (and (integerp status) (zerop status))))
          (if browse
              (browse-url-of-file output)
            (find-file-other-window output))
        (pop-to-buffer buffer)
        (user-error "mmdc failed")))))

;;;###autoload
(defun markdown-preview-buffer (&optional browse)
  "Render current Markdown file to HTML.
With prefix argument BROWSE, open the HTML in the browser."
  (interactive "P")
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (when (and (buffer-modified-p)
             (y-or-n-p "Save buffer before rendering? "))
    (save-buffer))
  (let* ((renderer (or (executable-find "mdrender")
                       markdown-preview-renderer))
         (output (concat (file-name-sans-extension buffer-file-name) ".html"))
         (buffer (get-buffer-create "*markdown render*")))
    (unless (file-executable-p renderer)
      (user-error "Missing mdrender CLI"))
    (with-current-buffer buffer
      (erase-buffer))
    (let ((status (call-process renderer nil buffer nil buffer-file-name output)))
      (if (and (integerp status) (zerop status))
          (if browse
              (browse-url-of-file output)
            (find-file-other-window output))
        (pop-to-buffer buffer)
        (user-error "mdrender failed")))))

;;;###autoload
(define-minor-mode markdown-preview-mode
  "Preview Markdown files and Mermaid blocks."
  :group 'markdown-preview
  :lighter " MdPrev"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-x m") #'markdown-preview-mermaid-block)
            (define-key map (kbd "C-c C-x p") #'markdown-preview-buffer)
            map))

(provide 'markdown-preview-mode)
