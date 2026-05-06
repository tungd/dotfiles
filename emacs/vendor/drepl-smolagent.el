;;; drepl-smolagent.el --- SmolAgent shell implemented via dREPL -*- lexical-binding: t; -*-

;; This is a local proof-of-concept package.

;;; Commentary:

;; `drepl-smolagent' starts a small dREPL kernel backed by Hugging Face
;; smolagents.  `drepl-smolagent-cells' opens a scratch buffer where
;; plain "# %%" cells dispatch to the same dREPL session.  Cell bodies
;; beginning with "!" run as shell commands; other cell bodies are sent
;; as agent prompts.

;;; Code:

(require 'cl-lib)
(require 'ansi-color)
(require 'auth-source)
(require 'comint-mime)
(require 'json)
(require 'pulse)

(declare-function code-cells--bounds "code-cells" (&optional count use-region no-header))

(defvar display-comint-buffer-action
  (append display-buffer--same-window-action '((category . comint)))
  "Display action for Comint buffers.
Compatibility binding for dREPL 0.4 on Emacs builds where this
variable is no longer predefined.")

(require 'drepl)
(require 'subr-x)

(defgroup drepl-smolagent nil
  "SmolAgent shell implemented via dREPL."
  :group 'drepl)

(defface drepl-smolagent-result-header
  '((t :foreground "gray55" :weight bold :underline nil :overline nil :box nil))
  "Face for SmolAgent inline result headers.")

(defface drepl-smolagent-result
  '((((class color) (background dark))
     :foreground "gray72" :background "#171717" :extend t
     :underline nil :overline nil :box nil)
    (((class color) (background light))
     :foreground "gray25" :background "#f1f1ee" :extend t
     :underline nil :overline nil :box nil)
    (t :foreground "gray55" :underline nil :overline nil :box nil))
  "Face for SmolAgent inline result text.")

(defface drepl-smolagent-result-heading
  '((t :inherit drepl-smolagent-result :weight bold))
  "Face for Markdown headings in SmolAgent results.")

(defface drepl-smolagent-result-strong
  '((t :inherit drepl-smolagent-result :weight bold))
  "Face for Markdown strong text in SmolAgent results.")

(defface drepl-smolagent-result-emphasis
  '((t :inherit drepl-smolagent-result :slant italic))
  "Face for Markdown italic/emphasis text in SmolAgent results.")

(defface drepl-smolagent-result-code
  '((t :inherit drepl-smolagent-result :foreground "gray78"))
  "Face for Markdown code spans and blocks in SmolAgent results.")

(defface drepl-smolagent-result-code-block
  '((((class color) (background dark))
     :inherit fixed-pitch :background "#111111" :extend t)
    (((class color) (background light))
     :inherit fixed-pitch :background "#e7e7e2" :extend t)
    (t :inherit fixed-pitch))
  "Face for Markdown fenced code blocks in SmolAgent results.")

(defface drepl-smolagent-result-link
  '((t :inherit drepl-smolagent-result :underline t))
  "Face for Markdown links in SmolAgent results.")

(defface drepl-smolagent-result-table
  '((t :inherit drepl-smolagent-result-code))
  "Face for Markdown tables in SmolAgent results.")

(defface drepl-smolagent-result-quote
  '((t :inherit drepl-smolagent-result :slant italic :foreground "gray65"))
  "Face for Markdown blockquotes in SmolAgent results.")

(defface drepl-smolagent-result-toggle
  '((t :foreground "gray65" :weight bold :underline nil :overline nil :box nil))
  "Face for SmolAgent inline result toggle controls.")

(defface drepl-smolagent-activity
  '((t :foreground "gray60" :slant italic :underline nil :overline nil :box nil))
  "Face for ephemeral SmolAgent activity text.")

(defcustom drepl-smolagent-program "uv"
  "Program used to run the SmolAgent dREPL kernel."
  :type 'string)

(defcustom drepl-smolagent-python "3.13"
  "Python version passed to `uv run --python'."
  :type 'string)

(defcustom drepl-smolagent-environment nil
  "Extra environment variables for the SmolAgent dREPL process.
This can be a list of strings of the form NAME=VALUE, or a
function called with no arguments that returns such a list."
  :type '(choice (repeat string) function))

(defcustom drepl-smolagent-context-max-chars 80000
  "Maximum number of characters to send when loading buffer context.
When the rendered transcript is longer than this, keep the tail,
which is usually the most relevant part of an interactive session."
  :type 'natnum)

(defcustom drepl-smolagent-shell-result-max-chars 4000
  "Maximum characters shown inline for successful shell cell output.
The full output remains available in memory and can be toggled from
the result overlay.  Set this to 0 to always show full output."
  :type 'natnum)

(defcustom drepl-smolagent-result-display 'insert
  "How SmolAgent cell results are displayed.
The value `insert' uses real buffer text with text properties, which
scrolls more predictably.  The value `overlay' uses virtual text via
overlay `after-string'."
  :type '(choice (const :tag "Inserted result blocks" insert)
                 (const :tag "Overlay after-string" overlay)))

(defcustom drepl-smolagent-pulse-activity t
  "Whether to briefly highlight the running activity line when it changes."
  :type 'boolean)

(defcustom drepl-smolagent-render-markdown t
  "Whether to render simple Markdown in SmolAgent result blocks.
This only changes text properties in the displayed result.  The
stored run text remains the original Markdown."
  :type 'boolean)

(defcustom drepl-smolagent-markdown-code-language-modes
  '(("bash" sh-mode)
    ("c" c-mode)
    ("c++" c++-mode)
    ("cpp" c++-mode)
    ("css" css-mode)
    ("el" emacs-lisp-mode)
    ("elisp" emacs-lisp-mode)
    ("emacs-lisp" emacs-lisp-mode)
    ("go" go-ts-mode go-mode)
    ("html" html-mode)
    ("javascript" js-mode)
    ("js" js-mode)
    ("json" js-json-mode json-mode)
    ("lisp" lisp-mode)
    ("markdown" markdown-mode)
    ("md" markdown-mode)
    ("python" python-ts-mode python-mode)
    ("py" python-ts-mode python-mode)
    ("ruby" ruby-mode)
    ("rust" rust-ts-mode rust-mode)
    ("shell" sh-mode)
    ("sh" sh-mode)
    ("toml" toml-ts-mode conf-toml-mode)
    ("tsx" tsx-ts-mode typescript-ts-mode)
    ("typescript" typescript-ts-mode typescript-mode)
    ("ts" typescript-ts-mode typescript-mode)
    ("xml" nxml-mode)
    ("yaml" yaml-ts-mode yaml-mode)
    ("yml" yaml-ts-mode yaml-mode))
  "Mapping from Markdown fenced-code language names to major modes.
Each entry is (LANG MODE...), and the first available MODE is used."
  :type '(repeat (cons string (repeat symbol))))

(defvar drepl-smolagent--start-file
  (expand-file-name "drepl-smolagent.py"
                    (if load-file-name
                        (file-name-directory load-file-name)
                      default-directory))
  "File name of the SmolAgent dREPL kernel.")

(defvar drepl-smolagent--last-run-id 0
  "Last generated SmolAgent cell run id.")

(defvar drepl-smolagent--runs (make-hash-table :test 'equal)
  "Map SmolAgent run ids to source buffer overlay state.")

(defvar drepl-smolagent--toggle-result-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'drepl-smolagent-toggle-result)
    (define-key map (kbd "RET") #'drepl-smolagent-toggle-result)
    map)
  "Keymap used by SmolAgent inline result toggle controls.")

(defconst drepl-smolagent--eval-and-new-cell-keys
  '("C-c C-<return>"
    "C-c C-RET"
    "C-c <C-return>"
    "C-c <return>"
    "C-c RET"
    "C-c C-m"
    "C-c C-j"
    "C-c C-n")
  "Key sequences that evaluate the current cell and create a new one.")

(defvar drepl-smolagent-cells-keys-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (key drepl-smolagent--eval-and-new-cell-keys)
      (define-key map (kbd key) #'drepl-smolagent-eval-and-new-cell))
    map)
  "Keymap for SmolAgent cell convenience bindings.")

(define-minor-mode drepl-smolagent-cells-keys-mode
  "Minor mode for SmolAgent cell convenience bindings."
  :lighter nil
  :keymap drepl-smolagent-cells-keys-mode-map)

;;;###autoload (autoload 'drepl-smolagent "drepl-smolagent" nil t)
(drepl--define drepl-smolagent :display-name "SmolAgent")

(cl-defmethod drepl--command ((_ drepl-smolagent))
  `(,drepl-smolagent-program
    "run" "--python" ,drepl-smolagent-python
    "--script" ,drepl-smolagent--start-file))

(defun drepl-smolagent--environment ()
  "Return extra environment variables for the SmolAgent process."
  (if (functionp drepl-smolagent-environment)
      (funcall drepl-smolagent-environment)
    drepl-smolagent-environment))

(defcustom drepl-smolagent-alibaba-glm5-api-base
  "https://coding-intl.dashscope.aliyuncs.com/v1"
  "OpenAI-compatible Alibaba Coding Plan endpoint used for GLM-5."
  :type 'string)

(defcustom drepl-smolagent-openai-user-agent
  "OpenAI/Go 3.22.0"
  "User-Agent sent by the OpenAI-compatible SmolAgent client.
Set this to the empty string to use the Python OpenAI SDK default."
  :type 'string)

(defcustom drepl-smolagent-zai-glm5-api-base
  "https://api.z.ai/api/paas/v4"
  "OpenAI-compatible Z.ai endpoint used for GLM-5."
  :type 'string)

(defun drepl-smolagent--auth-source-password (host)
  "Return the first auth-source password for HOST."
  (auth-source-pick-first-password :host host :max 1))

(defun drepl-smolagent-alibaba-glm5-environment ()
  "Return SmolAgent environment variables for Alibaba GLM-5.
The API key is read from the auth-source entry with host
\"alibaba\", matching `gptel-alibaba-coding-plan-setup'."
  (append
   `("SMOLAGENT_MODEL_CLASS=openai"
     "SMOLAGENT_MODEL=glm-5"
     ,(concat "SMOLAGENT_API_BASE=" drepl-smolagent-alibaba-glm5-api-base))
   (unless (string-empty-p drepl-smolagent-openai-user-agent)
     (list (concat "SMOLAGENT_USER_AGENT=" drepl-smolagent-openai-user-agent)))
   (when-let* ((key (drepl-smolagent--auth-source-password "alibaba")))
     (list (concat "SMOLAGENT_API_KEY=" key)))))

(defun drepl-smolagent-zai-glm5-environment ()
  "Return SmolAgent environment variables for Z.ai GLM-5.
The API key is read from the auth-source entry with host \"zai\"."
  (append
   `("SMOLAGENT_MODEL_CLASS=openai"
     "SMOLAGENT_MODEL=glm-5"
     ,(concat "SMOLAGENT_API_BASE=" drepl-smolagent-zai-glm5-api-base))
   (when-let* ((key (or (drepl-smolagent--auth-source-password "zai")
                        (drepl-smolagent--auth-source-password "z.ai"))))
     (list (concat "SMOLAGENT_API_KEY=" key)))))

(defun drepl-smolagent-alibaba-coding-plan-environment ()
  "Return the default SmolAgent Alibaba GLM-5 environment.
This compatibility alias keeps existing init files working while
using GLM-5 as the default agent model."
  (drepl-smolagent-alibaba-glm5-environment))

(cl-defmethod drepl--init ((repl drepl-smolagent))
  (let ((process-environment
         (append (drepl-smolagent--environment) process-environment)))
    (cl-call-next-method repl))
  (add-to-list 'ansi-osc-handlers '("5151" . drepl-smolagent--mime-osc-handler))
  (setq-local comint-prompt-regexp "^smolagent> "))

(cl-defmethod drepl--handle-notification ((repl drepl-smolagent) data)
  "Handle SmolAgent notifications from REPL."
  (pcase (alist-get 'op data)
    ("run-start"
     (drepl-smolagent--run-start
      (alist-get 'run_id data)
      (alist-get 'kind data)))
    ("run-end"
     (drepl-smolagent--run-end
      (alist-get 'run_id data)
      (or (alist-get 'status data) "done")))
    (_ (cl-call-next-method repl data))))

(defun drepl-smolagent--new-run-id ()
  "Return a new SmolAgent run id."
  (format "emacs-%s-%d"
          (format-time-string "%Y%m%d%H%M%S")
          (cl-incf drepl-smolagent--last-run-id)))

(defun drepl-smolagent--delete-overlay (overlay &rest _)
  "Delete SmolAgent result OVERLAY."
  (delete-overlay overlay))

(defun drepl-smolagent--remove-overlays (start end)
  "Remove SmolAgent result overlays between START and END."
  (remove-overlays start end 'drepl-smolagent-source t)
  (remove-overlays start end 'drepl-smolagent-result t))

(defun drepl-smolagent--delete-inserted-result (run)
  "Delete inserted result text for RUN."
  (when-let* ((start (plist-get run :result-start))
              (end (plist-get run :result-end))
              ((markerp start))
              ((markerp end))
              ((marker-buffer start))
              ((marker-buffer end)))
    (with-current-buffer (marker-buffer start)
      (let ((inhibit-read-only t)
            (buffer-undo-list t)
            (modified (buffer-modified-p)))
        (delete-region start end)
        (set-marker start nil)
        (set-marker end nil)
        (set-buffer-modified-p modified))))
  (plist-put run :result-start nil)
  (plist-put run :result-end nil))

(defun drepl-smolagent--delete-inserted-results (start end)
  "Delete inserted SmolAgent result text between START and END."
  (let ((pos start)
        ranges)
    (while (< pos end)
      (let ((next (next-single-property-change
                   pos 'drepl-smolagent-result nil end)))
        (when (get-text-property pos 'drepl-smolagent-result)
          (push (cons pos next) ranges))
        (setq pos next)))
    (let ((inhibit-read-only t)
          (buffer-undo-list t)
          (modified (buffer-modified-p)))
      (dolist (range ranges)
        (delete-region (car range) (cdr range)))
      (set-buffer-modified-p modified))))

(defun drepl-smolagent--remove-results (start end)
  "Remove SmolAgent overlays and inserted result text between START and END."
  (drepl-smolagent--remove-overlays start end)
  (drepl-smolagent--delete-inserted-results start end))

(defun drepl-smolagent--buffer-substring-without-results (start end)
  "Return buffer text between START and END without inserted result blocks."
  (let ((pos start)
        chunks)
    (while (< pos end)
      (let ((next (next-single-property-change
                   pos 'drepl-smolagent-result nil end)))
        (unless (get-text-property pos 'drepl-smolagent-result)
          (push (buffer-substring-no-properties pos next) chunks))
        (setq pos next)))
    (apply #'concat (nreverse chunks))))

(defun drepl-smolagent--kind-from-directive (directive)
  "Return a display kind from SmolAgent DIRECTIVE."
  (string-remove-prefix "%%" (or directive "agent")))

(defun drepl-smolagent--register-run (run-id start end &optional kind)
  "Register RUN-ID as the current evaluation for START to END."
  (drepl-smolagent--remove-results start end)
  (let* ((start-marker (copy-marker start))
         (end-marker (copy-marker end))
         (overlay (make-overlay start-marker end-marker nil t)))
    (overlay-put overlay 'drepl-smolagent-source t)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'modification-hooks '(drepl-smolagent--delete-overlay))
    (overlay-put overlay 'insert-in-front-hooks '(drepl-smolagent--delete-overlay))
    (overlay-put overlay 'insert-behind-hooks '(drepl-smolagent--delete-overlay))
    (puthash run-id
             (list :buffer (current-buffer)
                   :start start-marker
                   :end end-marker
                   :overlay overlay
                   :status "Queued"
                   :kind kind
                   :expanded nil
                   :result-start nil
                   :result-end nil
                   :activity ""
                   :mime-type nil
                   :pulse nil
                   :text "")
             drepl-smolagent--runs)
    (drepl-smolagent--render-run run-id)))

(defun drepl-smolagent--run-start (run-id kind)
  "Mark RUN-ID as started with KIND."
  (when-let* ((run (and run-id (gethash run-id drepl-smolagent--runs))))
    (plist-put run :status (format "Running %s" (or kind "cell")))
    (plist-put run :kind kind)
    (plist-put run :activity "")
    (plist-put run :text "")
    (drepl-smolagent--render-run run-id)))

(defun drepl-smolagent--run-end (run-id status)
  "Mark RUN-ID as ended with STATUS."
  (when-let* ((run (and run-id (gethash run-id drepl-smolagent--runs))))
    (plist-put run :status (if (string= status "ok") "Done" status))
    (plist-put run :activity "")
    (drepl-smolagent--render-run run-id)))

(defun drepl-smolagent--agent-run-p (run)
  "Return non-nil when RUN is an agent run."
  (string= (plist-get run :kind) "agent"))

(defun drepl-smolagent--update-activity (run-id run text)
  "Update RUN's ephemeral activity for RUN-ID using TEXT."
  (let ((activity (string-trim (or text ""))))
    (unless (string-empty-p activity)
      (plist-put run :activity activity)
      (plist-put run :pulse t)
      (drepl-smolagent--render-run run-id)
      t)))

(defun drepl-smolagent--append-output (run-id text &optional stream mime-type)
  "Append TEXT to inline result for RUN-ID.
STREAM is the SmolAgent MIME stream name.  Agent step streams are
kept as ephemeral activity, not as transcript/result text."
  (when-let* ((run (and run-id (gethash run-id drepl-smolagent--runs))))
    (cond
     ((string= stream "step")
      (drepl-smolagent--update-activity run-id run text))
     ((and (drepl-smolagent--agent-run-p run)
           (member stream '("stdout" nil)))
      t)
     (t
      (when (string= stream "result")
        (plist-put run :activity "")
        (plist-put run :mime-type mime-type))
      (plist-put run :text (concat (plist-get run :text) text))
      (drepl-smolagent--render-run run-id)
      t))))

(defun drepl-smolagent--shell-run-p (run)
  "Return non-nil when RUN is a shell run."
  (member (plist-get run :kind) '("sh" "shell" "bash" "zsh")))

(defun drepl-smolagent--toggle-button (run-id expanded hidden)
  "Return a toggle button string for RUN-ID.
EXPANDED means the full result is currently shown.  HIDDEN is the
number of hidden characters in collapsed mode."
  (let ((label (if expanded
                   "[collapse output]"
                 (format "[show full: %d hidden chars]" hidden))))
    (propertize label
                'face 'drepl-smolagent-result-toggle
                'mouse-face 'highlight
                'help-echo "Toggle full shell output"
                'drepl-smolagent-run-id run-id
                'local-map drepl-smolagent--toggle-result-map)))

(defun drepl-smolagent--markdown-mask (string start end &optional display)
  "In STRING, visually replace START to END with DISPLAY or nothing."
  (when (< start end)
    (add-text-properties
     start end
     (list 'display (or display "")
           'rear-nonsticky '(display))
     string)))

(defun drepl-smolagent--line-end-with-newline (string end)
  "Return END advanced over a trailing newline in STRING when present."
  (if (and (< end (length string))
           (= (aref string end) ?\n))
      (1+ end)
    end))

(defun drepl-smolagent--face-list (face)
  "Return FACE as a list suitable for a face text property."
  (cond
   ((null face) nil)
   ((and (listp face) (keywordp (car face))) (list face))
   ((listp face) face)
   (t (list face))))

(defun drepl-smolagent--markdown-language-mode (language)
  "Return the first available major mode for Markdown code LANGUAGE."
  (let* ((lang (downcase (or language "")))
         (entry (assoc lang drepl-smolagent-markdown-code-language-modes))
         (modes (cdr entry)))
    (or (cl-find-if #'fboundp modes)
        (let ((candidate (intern-soft (concat lang "-ts-mode"))))
          (and candidate (fboundp candidate) candidate))
        (let ((candidate (intern-soft (concat lang "-mode"))))
          (and candidate (fboundp candidate) candidate)))))

(defun drepl-smolagent--fontify-code (code language)
  "Return CODE with native font-lock faces for LANGUAGE when possible."
  (let ((mode (drepl-smolagent--markdown-language-mode language)))
    (if (not mode)
        code
      (condition-case nil
          (with-temp-buffer
            (insert code)
            (let ((font-lock-verbose nil)
                  (inhibit-message t))
              (delay-mode-hooks
                (funcall mode))
              (font-lock-ensure (point-min) (point-max)))
            (buffer-substring (point-min) (point-max)))
        (error code)))))

(defun drepl-smolagent--apply-code-fontification (string start end language)
  "Apply fenced-code block styling to STRING from START to END.
LANGUAGE is the Markdown fence language name."
  (when (< start end)
    (let* ((code (substring string start end))
           (fontified (drepl-smolagent--fontify-code code language))
           (limit (min (length code) (length fontified))))
      (add-text-properties
       start end
       '(drepl-smolagent-code-block t)
       string)
      (add-face-text-property
       start end
       'drepl-smolagent-result-code-block t string)
      (dotimes (index limit)
        (let ((face (get-text-property index 'face fontified)))
          (when face
            (let* ((pos (+ start index))
                   (existing (get-text-property pos 'face string)))
              (put-text-property
               pos (1+ pos) 'face
               (append (drepl-smolagent--face-list face)
                       (drepl-smolagent--face-list existing))
               string))))))))

(defun drepl-smolagent--render-markdown (string)
  "Return STRING with lightweight Markdown syntax masked for display."
  (let ((case-fold-search nil)
        (pos 0))
    (while (string-match "^```[ \t]*\\([^` \t\r\n]*\\).*$" string pos)
      (let* ((open-start (match-beginning 0))
             (open-end (drepl-smolagent--line-end-with-newline
                        string (match-end 0)))
             (language (match-string 1 string))
             (content-start open-end))
        (if (string-match "^```[ \t]*$" string content-start)
            (let* ((close-start (match-beginning 0))
                   (close-end (drepl-smolagent--line-end-with-newline
                               string (match-end 0))))
              (drepl-smolagent--markdown-mask string open-start open-end)
              (drepl-smolagent--markdown-mask string close-start close-end)
              (drepl-smolagent--apply-code-fontification
               string content-start close-start language)
              (setq pos close-end))
          (drepl-smolagent--markdown-mask string open-start open-end)
          (drepl-smolagent--apply-code-fontification
           string content-start (length string) language)
          (setq pos open-end))))
    (setq pos 0)
    (while (string-match "^\\(#+\\)[ \t]+\\(.+\\)$" string pos)
      (unless (get-text-property (match-beginning 0)
                                 'drepl-smolagent-code-block
                                 string)
        (drepl-smolagent--markdown-mask
         string (match-beginning 1) (match-beginning 2))
        (add-face-text-property
         (match-beginning 2) (match-end 2)
         'drepl-smolagent-result-heading t string))
      (setq pos (match-end 0)))
    (setq pos 0)
    (while (string-match "^\\([ \t]*\\)\\([-+*]\\)\\([ \t]+\\)" string pos)
      (unless (get-text-property (match-beginning 0)
                                 'drepl-smolagent-code-block
                                 string)
        (drepl-smolagent--markdown-mask
         string (match-beginning 2) (match-end 3) "• "))
      (setq pos (match-end 0)))
    (setq pos 0)
    (while (string-match "`\\([^`\n]+\\)`" string pos)
      (unless (get-text-property (match-beginning 0)
                                 'drepl-smolagent-code-block
                                 string)
        (drepl-smolagent--markdown-mask
         string (match-beginning 0) (match-beginning 1))
        (drepl-smolagent--markdown-mask
         string (match-end 1) (match-end 0))
        (add-face-text-property
         (match-beginning 1) (match-end 1)
         'drepl-smolagent-result-code t string))
      (setq pos (match-end 0)))
    (setq pos 0)
    (while (string-match "\\*\\*\\([^*\n]+\\)\\*\\*" string pos)
      (unless (get-text-property (match-beginning 0)
                                 'drepl-smolagent-code-block
                                 string)
        (drepl-smolagent--markdown-mask
         string (match-beginning 0) (match-beginning 1))
        (drepl-smolagent--markdown-mask
         string (match-end 1) (match-end 0))
        (add-face-text-property
         (match-beginning 1) (match-end 1)
         'drepl-smolagent-result-strong t string))
      (setq pos (match-end 0)))
    string))

(defun drepl-smolagent--markdown-result-p (run)
  "Return non-nil when RUN's result should be rendered as Markdown."
  (and drepl-smolagent-render-markdown
       (drepl-smolagent--agent-run-p run)
       (string-match-p "markdown" (or (plist-get run :mime-type) ""))))

(defun drepl-smolagent--propertize-result (run text)
  "Return TEXT styled for RUN's result block."
  (let ((rendered (propertize (ansi-color-apply text)
                              'face 'drepl-smolagent-result)))
    (if (drepl-smolagent--markdown-result-p run)
        (drepl-smolagent--render-markdown rendered)
      rendered)))

(defun drepl-smolagent--result-body (run-id run text done)
  "Return propertized inline result body for RUN-ID, RUN, TEXT and DONE."
  (let ((base (lambda (value)
                (drepl-smolagent--propertize-result run value))))
    (if (and done
             (drepl-smolagent--shell-run-p run)
             (not (plist-get run :expanded))
             (> drepl-smolagent-shell-result-max-chars 0)
             (> (length text) drepl-smolagent-shell-result-max-chars))
        (let* ((limit drepl-smolagent-shell-result-max-chars)
               (shown (substring text 0 limit))
               (hidden (- (length text) limit)))
          (concat (funcall base shown)
                  "\n\n"
                  (drepl-smolagent--toggle-button run-id nil hidden)))
      (concat
       (funcall base text)
       (when (and done
                  (drepl-smolagent--shell-run-p run)
                  (plist-get run :expanded)
                  (> drepl-smolagent-shell-result-max-chars 0)
                  (> (length text) drepl-smolagent-shell-result-max-chars))
         (concat "\n\n" (drepl-smolagent--toggle-button run-id t 0)))))))

(defun drepl-smolagent--activity-body (run activity)
  "Return propertized ephemeral activity text for RUN."
  (let ((prefix (if (drepl-smolagent--agent-run-p run)
                    "Agent running"
                  "Running")))
    (propertize
     (if (string-empty-p activity)
         (concat prefix "...")
       (format "%s: %s" prefix activity))
     'face 'drepl-smolagent-activity)))

(defun drepl-smolagent--display-string (run-id run)
  "Return display string for RUN-ID and RUN."
  (let* ((status (plist-get run :status))
         (text (string-trim-right (or (plist-get run :text) "")))
         (activity (string-trim-right (or (plist-get run :activity) "")))
         (done (string= status "Done"))
         (running (string-prefix-p "Running" status))
         (shell-done (and done (drepl-smolagent--shell-run-p run)))
         (header
          (cond
           ((and done (not (string-empty-p text))) "")
           ((and running (drepl-smolagent--agent-run-p run)) "")
           (done "Done\n")
           ((string-prefix-p "exit" status) (format "[%s]\n" status))
           (t (format "%s\n" status))))
         (body
          (cond
           ((not (string-empty-p text))
            (drepl-smolagent--result-body run-id run text done))
           ((and running (drepl-smolagent--agent-run-p run))
            (drepl-smolagent--activity-body run activity))
           (t ""))))
    (concat
     "\n"
     (unless (string-empty-p header)
       (propertize header 'face 'drepl-smolagent-result-header))
     body
     (if shell-done "\n\n" "\n"))))

(defun drepl-smolagent--insert-result (run-id run display)
  "Insert DISPLAY as real result text for RUN-ID and RUN."
  (drepl-smolagent--delete-inserted-result run)
  (when-let* ((buffer (plist-get run :buffer))
              ((buffer-live-p buffer))
              (end (plist-get run :end))
              ((markerp end))
              ((marker-buffer end)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (buffer-undo-list t)
            (modified (buffer-modified-p))
            start-marker end-marker)
        (save-excursion
          (goto-char end)
          (setq start-marker (copy-marker (point)))
          (insert
           (propertize display
                       'drepl-smolagent-result t
                       'drepl-smolagent-run-id run-id
                       'read-only t
                       'front-sticky nil
                       'rear-nonsticky t))
          (setq end-marker (copy-marker (point))))
        (plist-put run :result-start start-marker)
        (plist-put run :result-end end-marker)
        (set-buffer-modified-p modified)))))

(defun drepl-smolagent--pulse-run-result (run)
  "Pulse RUN's inserted result block when possible."
  (when-let* (((and drepl-smolagent-pulse-activity
                    (fboundp 'pulse-momentary-highlight-region)))
              (start (plist-get run :result-start))
              (end (plist-get run :result-end))
              ((markerp start))
              ((markerp end))
              ((marker-buffer start))
              ((marker-buffer end)))
    (with-current-buffer (marker-buffer start)
      (pulse-momentary-highlight-region start end 'highlight))))

(defun drepl-smolagent--render-run (run-id)
  "Render inline overlay for RUN-ID."
  (when-let* ((run (gethash run-id drepl-smolagent--runs))
              (buffer (plist-get run :buffer))
              ((buffer-live-p buffer))
              (overlay (plist-get run :overlay)))
    (with-current-buffer buffer
      (let ((display (drepl-smolagent--display-string run-id run)))
        (if (eq drepl-smolagent-result-display 'overlay)
            (progn
              (drepl-smolagent--delete-inserted-result run)
              (overlay-put overlay 'after-string display)
              (overlay-put overlay 'drepl-smolagent-result t))
          (overlay-put overlay 'after-string nil)
          (drepl-smolagent--insert-result run-id run display))
        (when (plist-get run :pulse)
          (plist-put run :pulse nil)
          (drepl-smolagent--pulse-run-result run))))))

(defun drepl-smolagent--run-id-from-event (event)
  "Return SmolAgent run id attached to EVENT, or nil."
  (when (mouse-event-p event)
    (when-let* ((string-position (posn-string (event-end event)))
                (string (car string-position))
                (position (cdr string-position)))
      (get-text-property position 'drepl-smolagent-run-id string))))

(defun drepl-smolagent--run-id-at-point ()
  "Return the SmolAgent run id whose source overlay contains point."
  (let ((point (point))
        (run-at-point (get-text-property (point) 'drepl-smolagent-run-id)))
    (cl-labels
        ((find-run
          (start end)
          (maphash
           (lambda (run-id run)
             (when-let* ((buffer (plist-get run :buffer))
                         ((eq buffer (current-buffer)))
                         (run-start (marker-position (plist-get run :start)))
                         (run-end (marker-position (plist-get run :end)))
                         ((<= run-start end))
                         ((<= start run-end)))
               (setq run-at-point run-id)))
           drepl-smolagent--runs)))
      (unless run-at-point
        (find-run point point))
      (unless run-at-point
        (when (fboundp 'code-cells--bounds)
          (pcase-let ((`(,start ,end) (code-cells--bounds 1 nil nil)))
            (find-run start end))))
      run-at-point)))

(defun drepl-smolagent-toggle-result (&optional event)
  "Toggle the current SmolAgent inline result between collapsed and expanded."
  (interactive (list last-nonmenu-event))
  (let* ((run-id (or (drepl-smolagent--run-id-from-event event)
                     (drepl-smolagent--run-id-at-point)))
         (run (and run-id (gethash run-id drepl-smolagent--runs))))
    (unless run
      (user-error "No SmolAgent result at point"))
    (puthash run-id
             (plist-put run :expanded (not (plist-get run :expanded)))
             drepl-smolagent--runs)
    (drepl-smolagent--render-run run-id)))

(defun drepl-smolagent-clear-results ()
  "Remove SmolAgent inline result overlays from the current buffer."
  (interactive)
  (drepl-smolagent--remove-results (point-min) (point-max)))

(defun drepl-smolagent--mime-decode (text)
  "Decode a comint-mime OSC payload TEXT.
Return a cons cell (HEADER . DATA), or nil when TEXT cannot be decoded."
  (when (string-match "[^\n]*\n?" text)
    (let* ((payload (substring text (match-end 0)))
           (header (json-parse-string
                    (match-string 0 text)
                    :object-type 'alist
                    :null-object nil))
           (data (base64-decode-string payload)))
      (cons header data))))

(defun drepl-smolagent--mime-osc-handler (cmd text)
  "Route SmolAgent MIME payloads or delegate CMD and TEXT to `comint-mime'."
  (let* ((decoded (ignore-errors (drepl-smolagent--mime-decode text)))
         (header (car-safe decoded))
         (data (cdr-safe decoded))
         (run-id (alist-get 'run_id header))
         (stream (alist-get 'stream header))
         (mime-type (alist-get 'type header)))
    (if (and run-id
             data
             (drepl-smolagent--append-output
              run-id
              (decode-coding-string data 'utf-8)
              stream
              mime-type))
        nil
      (comint-mime-osc-handler cmd text))))

(defun drepl-smolagent--cell-command (code &optional exclude-start exclude-end)
  "Return a cons cell (DIRECTIVE . BODY) for cell CODE.
EXCLUDE-START and EXCLUDE-END are used when a load cell asks for
the current buffer transcript without including itself."
  (let ((trimmed (string-trim-left code)))
    (cond
     ((string-prefix-p "!" trimmed)
      (cons "%%sh" (substring trimmed 1)))
     ((string-match-p "\\`/load\\(?:\\s-+buffer\\)?\\s-*\\'" trimmed)
      (cons "%%load"
            (drepl-smolagent--buffer-context exclude-start exclude-end)))
     (t
      (cons "%%agent" code)))))

(defun drepl-smolagent--ensure-code-cells ()
  "Ensure code-cells helpers are available."
  (unless (fboundp 'code-cells--bounds)
    (require 'code-cells nil t))
  (unless (fboundp 'code-cells--bounds)
    (user-error "code-cells is not loaded")))

(defun drepl-smolagent--live-runs ()
  "Return live inline result runs for the current buffer."
  (let (runs)
    (maphash
     (lambda (run-id run)
       (when-let* ((buffer (plist-get run :buffer))
                   ((eq buffer (current-buffer)))
                   (overlay (plist-get run :overlay))
                   ((overlay-buffer overlay)))
         (push (cons run-id run) runs)))
     drepl-smolagent--runs)
    (sort runs
          (lambda (a b)
            (< (marker-position (plist-get (cdr a) :start))
               (marker-position (plist-get (cdr b) :start)))))))

(defun drepl-smolagent--truncate-context (text)
  "Return TEXT truncated according to `drepl-smolagent-context-max-chars'."
  (if (or (zerop drepl-smolagent-context-max-chars)
          (<= (length text) drepl-smolagent-context-max-chars))
      text
    (concat
     (format "[Earlier context omitted; kept last %d characters.]\n\n"
             drepl-smolagent-context-max-chars)
     (substring text (- (length text) drepl-smolagent-context-max-chars)))))

(defun drepl-smolagent--buffer-context (&optional exclude-start exclude-end)
  "Return the current buffer source and live inline results as transcript.
When EXCLUDE-START and EXCLUDE-END are non-nil, omit that buffer
region from the source section."
  (let* ((source (if (and exclude-start exclude-end)
                     (concat
                      (drepl-smolagent--buffer-substring-without-results
                       (point-min) exclude-start)
                      (drepl-smolagent--buffer-substring-without-results
                       exclude-end (point-max)))
                   (drepl-smolagent--buffer-substring-without-results
                    (point-min) (point-max))))
         (results
          (mapconcat
           #'identity
           (delq
            nil
            (mapcar
             (pcase-lambda (`(,run-id . ,run))
               (let ((text (string-trim-right (or (plist-get run :text) ""))))
                 (unless (string-empty-p text)
                   (format "Run %s, line %d, status %s:\n%s"
                           run-id
                           (line-number-at-pos (marker-position (plist-get run :start)))
                           (or (plist-get run :status) "unknown")
                           text))))
             (drepl-smolagent--live-runs)))
           "\n\n")))
    (drepl-smolagent--truncate-context
     (string-trim-right
      (concat
       "# Buffer source\n\n"
       source
       (unless (string-empty-p (string-trim results))
         (concat "\n\n# Inline results\n\n" results)))))))

(defun drepl-smolagent-eval-region (start end)
  "Evaluate the SmolAgent cell body between START and END."
  (let* ((code (drepl-smolagent--buffer-substring-without-results start end))
         (command (drepl-smolagent--cell-command code start end))
         (directive (car command))
         (body (cdr command))
         (run-id (drepl-smolagent--new-run-id)))
    (drepl-smolagent--register-run
     run-id start end (drepl-smolagent--kind-from-directive directive))
    (drepl-eval
     (string-join (list (format "%s --run-id %s" directive run-id) body) "\n"))
    run-id))

(defun drepl-smolagent-load-buffer-context ()
  "Load the current SmolAgent cells buffer as context for later prompts."
  (interactive)
  (drepl-eval (concat "%%load\n" (drepl-smolagent--buffer-context)))
  (message "Loaded SmolAgent buffer context"))

(defun drepl-smolagent-eval-and-new-cell ()
  "Evaluate the current SmolAgent cell and insert a new empty cell below."
  (interactive)
  (drepl-smolagent--ensure-code-cells)
  (pcase-let* ((`(,start ,body-end) (code-cells--bounds 1 t t))
               (`(,_cell-start ,cell-end) (code-cells--bounds 1 t nil))
               (run-id (drepl-smolagent-eval-region start body-end))
               (run (gethash run-id drepl-smolagent--runs))
               (result-end (plist-get run :result-end)))
    (goto-char (if (and (markerp result-end)
                        (marker-buffer result-end))
                   result-end
                 cell-end))
    (let ((inhibit-read-only t))
      (unless (bolp)
        (insert "\n"))
      (unless (looking-back "\n\n" nil)
        (insert "\n"))
      (insert "# %%\n"))))

;;;###autoload
(define-derived-mode drepl-smolagent-cells-mode text-mode "SmolAgent Cells"
  "Major mode for SmolAgent proof-of-concept cell buffers."
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local code-cells-boundary-regexp "^#\\s-*\\(%+\\)")
  (setq-local code-cells-eval-region-commands
              '((drepl-smolagent-cells-mode . drepl-smolagent-eval-region)
                (drepl--current . drepl-eval-region)))
  (setq-local truncate-lines nil)
  (visual-line-mode 1)
  (require 'code-cells nil t)
  (when (fboundp 'code-cells-mode)
    (code-cells-mode 1))
  (drepl-smolagent-cells-keys-mode 1))

(define-key drepl-smolagent-cells-mode-map
            (kbd "C-c C-o")
            #'drepl-smolagent-clear-results)
(define-key drepl-smolagent-cells-mode-map
            (kbd "C-c C-t")
            #'drepl-smolagent-toggle-result)
(define-key drepl-smolagent-cells-mode-map
            (kbd "C-c C-l")
            #'drepl-smolagent-load-buffer-context)
(dolist (key drepl-smolagent--eval-and-new-cell-keys)
  (define-key drepl-smolagent-cells-mode-map
              (kbd key)
              #'drepl-smolagent-eval-and-new-cell))

(defconst drepl-smolagent--sample-buffer
  "# %%
!pwd

# %%
Say hello from the smolagents dREPL proof of concept, then explain in one sentence what you can do.
"
  "Initial contents for `drepl-smolagent-cells'.")

;;;###autoload
(defun drepl-smolagent-cells ()
  "Open a code-cells scratch buffer associated with a SmolAgent dREPL."
  (interactive)
  (let ((repl-buffer (drepl--get-buffer-create 'drepl-smolagent t))
        (cell-buffer (get-buffer-create "*smolagent-cells*")))
    (pop-to-buffer cell-buffer)
    (unless (derived-mode-p 'drepl-smolagent-cells-mode)
      (drepl-smolagent-cells-mode))
    (setq-local drepl--current repl-buffer)
    (when (= (point-min) (point-max))
      (insert drepl-smolagent--sample-buffer)
      (goto-char (point-min)))
    (message "Associated this buffer with %s" (buffer-name repl-buffer))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.agent\\'" . drepl-smolagent-cells-mode))

(provide 'drepl-smolagent)

;;; drepl-smolagent.el ends here
