;;; tterm.el --- Terminal emulator using OCaml engine -*- lexical-binding: t; -*-

;; Terminal emulator backed by an in-process OCaml terminal engine.

(eval-and-compile
  (defconst tterm--directory
    (file-name-directory (or load-file-name buffer-file-name default-directory))
    "Directory containing tterm.el."))

(require 'cl-lib)
(require 'term)
(require 'tramp nil t)
(require 'tterm-bridge (expand-file-name "tterm-bridge" tterm--directory))

;;; Customization

(defcustom tterm-buffer-size 50000
  "Scrollback buffer size in lines."
  :type 'integer
  :group 'tterm)

(defcustom tterm-redraw-update-delay 0.025
  "Seconds between Emacs pulls while the terminal is changing."
  :type 'number
  :group 'tterm)

(defcustom tterm-redraw-idle-delay 0.125
  "Seconds between Emacs pulls after a redraw finds no terminal changes."
  :type 'number
  :group 'tterm)

(defcustom tterm-redraw-active-grace-delay 0.25
  "Seconds to keep fast redraw polling after the latest terminal change."
  :type 'number
  :group 'tterm)

(defcustom tterm-capture-refresh-idle-interval 2.0
  "Seconds between tmux capture resyncs for an idle visible terminal.
This is a recovery path for panes whose tmux control-mode output was missed
while the buffer was hidden or inactive."
  :type 'number
  :group 'tterm)

(defcustom tterm-attention-refresh-interval 2.0
  "Seconds between backend unread-notification polls while tterm buffers exist."
  :type 'number
  :group 'tterm)

(defcustom tterm-wheel-scroll-lines 3
  "Number of terminal scrollback lines to move per mouse wheel event."
  :type 'integer
  :group 'tterm)

(defcustom tterm-resize-while-minibuffer-active nil
  "Whether transient minibuffer layout changes should resize the PTY.
When nil, tterm keeps the child terminal size stable while minibuffer
completion windows are active."
  :type 'boolean
  :group 'tterm)

(defcustom tterm-resize-debounce-delay 0.25
  "Seconds to wait before applying automatic window-size resizes."
  :type 'number
  :group 'tterm)

(defconst tterm--built-in-fontset-fallbacks
  '(((#x00B7 . #x00B7) . "Menlo")
    ((#x2300 . #x23FF) . "Menlo")
    ((#x2500 . #x25FF) . "Menlo")
    ((#x2700 . #x27BF) . "Menlo"))
  "Built-in font fallbacks for terminal symbol ranges.")

(defconst tterm--symbol-display-substitutions
  '((#x23BF . "└"))
  "Display substitutions for terminal glyphs with poor font coverage.")

(defconst tterm--built-in-symbol-face-fallbacks
  '(((#x273B . #x273F) . "Menlo"))
  "Built-in per-character font faces for line-height-sensitive symbols.")

(defcustom tterm-fontset-fallbacks
  tterm--built-in-fontset-fallbacks
  "Fontset fallbacks installed for terminal symbol glyphs.
Each entry is (CHARACTERS . FONT-SPEC), as accepted by `set-fontset-font'.
This is useful for Claude Code spinner glyphs that otherwise fall back to
proportional symbol fonts with taller line boxes."
  :type '(repeat (cons sexp (choice string sexp)))
  :group 'tterm)

(defcustom tterm-symbol-face-fallbacks
  tterm--built-in-symbol-face-fallbacks
  "Per-character font faces for terminal symbols that can alter line height.
Each entry is (CHARACTERS . FONT), where CHARACTERS is either a character code
or an inclusive (MIN . MAX) range. Keep this list narrow; broad symbol fallback
belongs in `tterm-fontset-fallbacks'."
  :type '(repeat (cons sexp string))
  :group 'tterm)

(defface tterm-mode-line-normal
  '((t :inherit success))
  "Face for the normal mode-line indicator."
  :group 'tterm)

(defface tterm-mode-line-copy
  '((t :inherit font-lock-keyword-face))
  "Face for the copy mode-line indicator."
  :group 'tterm)

;;; Terminal state

(cl-defstruct tterm
  "State for a tterm terminal."
  id
  buffer
  rows
  cols
  displayed-version
  title
  cwd
  host
  window-id
  pane-id
  handle
  detached
  application-cursor
  alt-screen)

(defun tterm-set-application-cursor (term value)
  "Set TERM application-cursor mode to VALUE."
  (setf (tterm-application-cursor term) value))

(defun tterm-set-alt-screen (term value)
  "Set TERM alternate-screen mode to VALUE."
  (setf (tterm-alt-screen term) value))

(defun tterm-set-cwd (term value)
  "Set TERM current working directory to VALUE."
  (setf (tterm-cwd term) value))

(defvar-local desktop-save-buffer nil)

(defvar-local tterm--terminal nil
  "The tterm terminal struct for this buffer.")

(defvar-local tterm--redraw-timer nil
  "Pending redraw timer for this tterm buffer.")

(defvar-local tterm--redraw-request-timer nil
  "Pending one-shot redraw request timer.")

(defvar-local tterm--resize-timer nil
  "Pending coalesced window resize timer.")

(defvar-local tterm--resize-window-starts nil
  "Visible window state captured while automatic resize is settling.")

(defvar-local tterm--last-redraw-change-time nil
  "Last `float-time' value when a redraw pull found terminal changes.")

(defvar-local tterm--last-capture-refresh-time nil
  "Last `float-time' value when this buffer requested tmux capture resync.")

(defvar-local tterm--input-mode 'normal
  "Current terminal input mode.
Only `normal` is valid while copy mode is inactive.")

(defvar tterm-mode-map)
(defvar tterm-copy-mode-map)

(defvar tterm--mode-line-input-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'tterm-toggle-copy-mode)
    map)
  "Keymap for tterm's compact mode-line mode lighter.")

(defvar tterm--attention-refresh-timer nil
  "Timer that refreshes global tterm attention state.")

(defvar tterm--attention-unread-total 0
  "Total unread terminal notifications across backend windows.")

(defvar tterm--attention-windows nil
  "Dashboard window plists with unread terminal notifications.")

(defvar tterm--attention-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'tterm-jump-next-notification)
    map)
  "Mode-line keymap for tterm attention indicator.")

(defun tterm--buffers ()
  "Return live buffers currently using `tterm-mode'."
  (cl-remove-if-not
   (lambda (buffer)
     (with-current-buffer buffer
       (eq major-mode 'tterm-mode)))
   (buffer-list)))

(defun tterm--buffer-for-terminal-id (id)
  "Return the live tterm buffer currently attached to terminal ID."
  (cl-find-if
   (lambda (buffer)
     (with-current-buffer buffer
       (and (eq major-mode 'tterm-mode)
            (boundp 'tterm--terminal)
            tterm--terminal
            (= (tterm-id tterm--terminal) id))))
   (buffer-list)))

(defun tterm-switch-buffer ()
  "Switch to a tterm buffer, using completion over live tterm buffers."
  (interactive)
  (let ((items (mapcar #'buffer-name (tterm--buffers))))
    (if items
        (switch-to-buffer
         (completing-read "tterm: " items nil t))
      (user-error "No tterm buffers"))))

(defconst tterm--ansi-color-names
  ["#000000" "#cd0000" "#00cd00" "#cdcd00"
   "#0000ee" "#cd00cd" "#00cdcd" "#e5e5e5"
   "#7f7f7f" "#ff0000" "#00ff00" "#ffff00"
   "#5c5cff" "#ff00ff" "#00ffff" "#ffffff"]
  "Basic ANSI color names by palette index.")

(defconst tterm--ansi-color-face-suffixes
  ["black" "red" "green" "yellow"
   "blue" "magenta" "cyan" "white"
   "bright-black" "bright-red" "bright-green" "bright-yellow"
   "bright-blue" "bright-magenta" "bright-cyan" "bright-white"]
  "Face suffixes for ANSI palette indexes.")

(defconst tterm--term-color-faces
  [term-color-black term-color-red term-color-green term-color-yellow
   term-color-blue term-color-magenta term-color-cyan term-color-white
   term-color-bright-black term-color-bright-red term-color-bright-green
   term-color-bright-yellow term-color-bright-blue term-color-bright-magenta
   term-color-bright-cyan term-color-bright-white]
  "Built-in term face names used for ANSI palette colors.")

(defun tterm--terminal-color-face-symbol (kind index)
  "Return tterm terminal color face symbol for KIND and palette INDEX."
  (when (and (integerp index) (>= index 0)
             (< index (length tterm--ansi-color-face-suffixes)))
    (intern (format "tterm-%s-%s"
                    kind
                    (aref tterm--ansi-color-face-suffixes index)))))

(defun tterm--declare-terminal-color-faces ()
  "Declare tterm terminal color faces."
  (dotimes (index (length tterm--ansi-color-face-suffixes))
    (let ((suffix (aref tterm--ansi-color-face-suffixes index)))
      (dolist (kind '("fg" "bg"))
        (let ((face (tterm--terminal-color-face-symbol kind index)))
          (unless (facep face)
            (custom-declare-face
             face
             '((t nil))
             (format "Face for terminal %s palette color %s." kind suffix)
             :group 'tterm)))))))

(tterm--declare-terminal-color-faces)

(defface tterm-default
  '((t :inherit default))
  "Default face for terminal cells without explicit SGR colors."
  :group 'tterm)

(defun tterm--palette-source-face (index)
  "Return the Emacs face used as source for ANSI palette INDEX."
  (when (and (integerp index) (>= index 0) (< index 16))
    (let ((term-face (aref tterm--term-color-faces index)))
      (when (facep term-face)
        term-face))))

(defun tterm--palette-face (index attribute)
  "Return tterm face for ANSI palette INDEX and ATTRIBUTE."
  (pcase attribute
    (:foreground (tterm--terminal-color-face-symbol "fg" index))
    (:background (tterm--terminal-color-face-symbol "bg" index))
    (_ nil)))

(defun tterm--face-color (face attribute)
  "Return FACE color ATTRIBUTE, falling back through inherited faces."
  (let ((value (and face (face-attribute face attribute nil 'default))))
    (unless (memq value '(nil unspecified))
      value)))

(defun tterm--palette-color-name (index attribute)
  "Return ANSI palette INDEX color string for ATTRIBUTE.
ATTRIBUTE should be either `:foreground' or `:background'."
  (let ((face (tterm--palette-source-face index))
        (fallback (if (eq attribute :foreground) :background :foreground)))
    (or (tterm--face-color face attribute)
        (tterm--face-color face fallback)
        (tterm--xterm-256-color index))))

(defun tterm--default-color-name (attribute fallback-index)
  "Return terminal default color for ATTRIBUTE with FALLBACK-INDEX."
  (or (tterm--face-color 'default attribute)
      (tterm--palette-color-name fallback-index attribute)
      'unspecified))

(defun tterm--update-terminal-color-faces (&rest _)
  "Refresh tterm terminal color faces from Emacs terminal palette faces."
  (dotimes (index (length tterm--ansi-color-face-suffixes))
    (let ((fg-face (tterm--palette-face index :foreground))
          (bg-face (tterm--palette-face index :background))
          (fg-color (tterm--palette-color-name index :foreground))
          (bg-color (if (zerop index)
                        (tterm--default-color-name :background 0)
                      (tterm--palette-color-name index :background))))
      (when fg-face
        (set-face-attribute fg-face nil
                            :foreground (or fg-color 'unspecified)
                            :background 'unspecified))
      (when bg-face
        (set-face-attribute bg-face nil
                            :foreground 'unspecified
                            :background (or bg-color 'unspecified)))))
  (set-face-attribute 'tterm-default nil
                      :foreground (tterm--default-color-name :foreground 7)
                      :background (tterm--default-color-name :background 0))
  (when (fboundp 'tterm-bridge-mark-runtime-config-dirty)
    (tterm-bridge-mark-runtime-config-dirty)))

(tterm--update-terminal-color-faces)
(add-hook 'after-init-hook #'tterm--update-terminal-color-faces t)
(add-hook 'enable-theme-functions #'tterm--update-terminal-color-faces t)
(add-hook 'after-theme-changed-hook #'tterm--update-terminal-color-faces t)

(defun tterm--xterm-256-color (index)
  "Return an RGB color string for xterm 256-color palette INDEX."
  (cond
   ((and (>= index 0) (< index (length tterm--ansi-color-names)))
    (aref tterm--ansi-color-names index))
   ((and (>= index 16) (<= index 231))
    (let* ((n (- index 16))
           (levels [0 95 135 175 215 255])
           (r (aref levels (/ n 36)))
           (g (aref levels (/ (% n 36) 6)))
           (b (aref levels (% n 6))))
      (format "#%02x%02x%02x" r g b)))
   ((and (>= index 232) (<= index 255))
    (let ((level (+ 8 (* 10 (- index 232)))))
      (format "#%02x%02x%02x" level level level)))
   (t nil)))

(defun tterm--apply-color-name (color &optional attribute)
  "Return an Emacs color string for decoded apply-plan COLOR.
ATTRIBUTE selects the palette face attribute to use and defaults to
`:background'."
  (pcase color
    (`(palette . ,index)
     (tterm--palette-color-name index (or attribute :background)))
    (`[palette ,index]
     (tterm--palette-color-name index (or attribute :background)))
    (`(rgb ,r ,g ,b) (format "#%02x%02x%02x" r g b))
    (`[rgb ,r ,g ,b] (format "#%02x%02x%02x" r g b))
    (_ nil)))

(defun tterm--apply-palette-face (color attribute)
  "Return a symbolic palette face for decoded apply-plan COLOR."
  (pcase color
    (`(palette . ,index) (tterm--palette-face index attribute))
    (`[palette ,index] (tterm--palette-face index attribute))
    (_ nil)))

(defun tterm--apply-color-face (color attribute)
  "Return a face representation for decoded apply-plan COLOR ATTRIBUTE."
  (or (tterm--apply-palette-face color attribute)
      (let ((color-name (tterm--apply-color-name color attribute)))
        (when color-name
          (list attribute color-name)))))

(defun tterm--apply-foreground-face (color)
  "Return a foreground face representation for decoded apply-plan COLOR."
  (tterm--apply-color-face color :foreground))

(defun tterm--apply-background-face (color)
  "Return a background face representation for decoded apply-plan COLOR."
  (tterm--apply-color-face color :background))

(defun tterm--unibyte-high-byte-p (text)
  "Return non-nil when TEXT is unibyte and contains non-ASCII bytes."
  (and (not (multibyte-string-p text))
       (string-match-p "[\200-\377]" text)))

(defun tterm--decode-apply-text (text)
  "Decode apply-plan UTF-8 TEXT while preserving unibyte ASCII fast paths."
  (if (tterm--unibyte-high-byte-p text)
      (decode-coding-string text 'utf-8 t)
    text))

(defun tterm--decode-apply-cell-text (text cell-count)
  "Decode apply-plan UTF-8 TEXT known to represent CELL-COUNT cells.
When a unibyte payload has one byte per cell, it is already ASCII."
  (if (and (not (multibyte-string-p text))
           (not (tterm--unibyte-high-byte-p text))
           (= (string-bytes text) cell-count))
      text
    (tterm--decode-apply-text text)))

(defun tterm--apply-row-run-ascii-bytes (count cols)
  "Return byte count for an ASCII full-row run, or nil without COLS."
  (and cols
       (+ (* count cols) (max 0 (1- count)))))

(defun tterm--decode-apply-row-run-text (text count cols)
  "Decode full-row run TEXT for COUNT rows and COLS columns."
  (let ((ascii-bytes (tterm--apply-row-run-ascii-bytes count cols)))
    (if (and ascii-bytes
             (not (multibyte-string-p text))
             (not (tterm--unibyte-high-byte-p text))
             (= (string-bytes text) ascii-bytes))
        text
      (tterm--decode-apply-text text))))

(defun tterm--effective-fontset-fallbacks ()
  "Return custom fontset fallbacks plus any missing built-in terminal ranges."
  (let ((fallbacks (copy-sequence tterm-fontset-fallbacks)))
    (dolist (entry tterm--built-in-fontset-fallbacks)
      (unless (assoc (car entry) fallbacks)
        (setq fallbacks (append fallbacks (list entry)))))
    fallbacks))

(defun tterm--fontset-range-span (range)
  "Return the inclusive span of fontset RANGE."
  (cond
   ((consp range)
    (- (cdr range) (car range)))
   ((integerp range)
    0)
   (t
    most-positive-fixnum)))

(defun tterm--fontset-fallback-install-order (fallbacks)
  "Return FALLBACKS ordered for `prepend' installation.
Broad ranges must be installed before narrow ranges so the final prepend keeps
the narrow override first in the fontset."
  (cl-stable-sort
   (copy-sequence fallbacks)
   (lambda (left right)
     (> (tterm--fontset-range-span (car left))
        (tterm--fontset-range-span (car right))))))

(defun tterm--apply-symbol-display-substitutions (text start)
  "Apply configured display substitutions for inserted TEXT at START."
  (dolist (entry tterm--symbol-display-substitutions)
    (let ((needle (char-to-string (car entry)))
          (display (cdr entry))
          (offset 0)
          match)
      (while (setq match (string-match (regexp-quote needle) text offset))
        (put-text-property (+ start match) (1+ (+ start match))
                           'display display)
        (setq offset (1+ match))))))

(defun tterm--range-contains-char-p (range char)
  "Return non-nil when RANGE contains CHAR."
  (cond
   ((consp range)
    (and (>= char (car range))
         (<= char (cdr range))))
   ((integerp range)
    (= char range))
   (t nil)))

(defun tterm--symbol-face-font-for-char (char)
  "Return the configured symbol face font for CHAR, or nil."
  (let (font
        span)
    (dolist (entry tterm-symbol-face-fallbacks)
      (let ((range (car entry)))
        (when (tterm--range-contains-char-p range char)
          (let ((candidate-span (tterm--fontset-range-span range)))
            (when (or (null span)
                      (< candidate-span span))
              (setq font (cdr entry)
                    span candidate-span))))))
    font))

(defun tterm--prepend-symbol-font-face (face font)
  "Return FACE with FONT prepended as a family face."
  (let ((font-face `(:family ,font)))
    (cond
     ((null face)
      (list font-face))
     ((and (listp face) (keywordp (car face)))
      (list font-face face))
     ((listp face)
      (cons font-face face))
     (t
      (list font-face face)))))

(defun tterm--apply-symbol-face-fallback-for-char (char font text start)
  "Apply FONT face to occurrences of CHAR in inserted TEXT at START."
  (let ((needle (char-to-string char))
        (offset 0)
        match)
    (while (setq match (string-search needle text offset))
      (let ((pos (+ start match)))
        (put-text-property
         pos (1+ pos) 'face
         (tterm--prepend-symbol-font-face
          (get-text-property pos 'face)
          font)))
      (setq offset (1+ match)))))

(defun tterm--apply-symbol-face-fallbacks (text start)
  "Apply configured symbol face fallbacks to matched chars in TEXT.
This searches for configured glyphs instead of walking all inserted text, so
large scrollback inserts stay cheap when they do not contain those glyphs."
  (dolist (entry tterm-symbol-face-fallbacks)
    (let ((range (car entry))
          (font (cdr entry)))
      (cond
       ((consp range)
        (let ((char (car range)))
          (while (<= char (cdr range))
            (tterm--apply-symbol-face-fallback-for-char char font text start)
            (setq char (1+ char)))))
       ((integerp range)
        (tterm--apply-symbol-face-fallback-for-char range font text start))))))

(defun tterm--apply-symbol-font-fallbacks-for-text (text start end)
  "Apply terminal symbol display and narrow face fallbacks to inserted TEXT."
  (when (and (multibyte-string-p text) (< start end))
    (tterm--apply-symbol-display-substitutions text start)
    (tterm--apply-symbol-face-fallbacks text start)))

;;; Core functions

(defun tterm--remote-host-candidates ()
  "Return SSH host candidates for `tterm'."
  (let (hosts)
    (when (fboundp 'tramp-parse-sconfig)
      (dolist (entry (ignore-errors (tramp-parse-sconfig "~/.ssh/config")))
        (let ((host (cond
                     ((and (consp entry) (stringp (cadr entry)))
                      (cadr entry))
                     ((and (consp entry) (stringp (car entry)))
                      (car entry)))))
          (when (and host (not (string-match-p "[*?]" host)))
            (push host hosts)))))
    (delete-dups (sort hosts #'string<))))

(defun tterm-read-remote-host ()
  "Read a remote SSH host using normal Emacs completion."
  (completing-read "tterm host: " (tterm--remote-host-candidates) nil nil))

(defun tterm--normalize-start-cwd (cwd)
  "Return CWD in the form passed to tmux when creating a window."
  (directory-file-name cwd))

(defun tterm--cwd-for-host (host)
  "Return current directory mapped for HOST."
  (if (and (not (string= host "local"))
           (fboundp 'tramp-tramp-file-p)
           (tramp-tramp-file-p default-directory))
      (let ((vec (tramp-dissect-file-name default-directory)))
        (or (tramp-file-name-localname vec) "~"))
    (expand-file-name default-directory)))

(defun tterm--connect (rows cols host cwd)
  "Connect to tmux-backed terminal on HOST at CWD."
  (tterm-bridge-connect rows cols host cwd))

(defun tterm--new (rows cols &optional scrollback cwd)
  "Create a new local tmux-backed terminal. Returns terminal ID.
SCROLLBACK is accepted for compatibility with older test and bench helpers."
  (ignore scrollback)
  (tterm--connect rows cols "local"
                  (if (and cwd (not (string-empty-p cwd)))
                      (tterm--normalize-start-cwd (expand-file-name cwd))
                    "")))

(defun tterm--pull-apply-plan-ops (id displayed-version)
  "Pull apply-ready ops for terminal ID from DISPLAYED-VERSION.
Returns [BASE_VERSION TARGET_VERSION RESET OPS]."
  (let ((bytes (tterm-bridge-pull-apply-plan-bytes id displayed-version)))
    (car (read-from-string bytes))))

(defun tterm--write-input (id bytes)
  "Write input BYTES to terminal ID."
  (tterm--command id "write-input" bytes))

(defun tterm--paste-input (id bytes)
  "Paste BYTES to terminal ID."
  (tterm--command id "paste-input" bytes))

(defun tterm--bracketed-paste-enabled-p (id)
  "Return non-nil when terminal ID has enabled bracketed paste."
  (tterm-bridge-bracketed-paste-enabled-p id))

(defun tterm--resize (id rows cols)
  "Resize terminal ID to ROWS x COLS."
  (tterm--command id "resize" (format "%dx%d" rows cols)))

(defun tterm--destroy (id)
  "Destroy terminal ID and its tmux window."
  (tterm--command id "kill-window" ""))

(defun tterm--dispose-terminal-buffer ()
  "Dispose the current Attached Terminal Buffer without tmux commands."
  (when (fboundp 'tterm--stop-redraw-timer)
    (tterm--stop-redraw-timer))
  (when (fboundp 'tterm--stop-redraw-request-timer)
    (tterm--stop-redraw-request-timer))
  (when (fboundp 'tterm--stop-resize-timer)
    (tterm--stop-resize-timer))
  (setq-local tterm--terminal nil)
  (setq-local tterm--title nil)
  (setq-local tterm--last-redraw-change-time nil))

(defun tterm--detach-current-terminal ()
  "Detach the buffer-local terminal from Emacs, preserving its tmux window."
  (when tterm--terminal
    (tterm--command (tterm-id tterm--terminal) "detach" "")
    (setf (tterm-detached tterm--terminal) t))
  (tterm--dispose-terminal-buffer))

(defun tterm--kill-current-terminal-window ()
  "Kill the current terminal's tmux window and dispose its buffer attachment."
  (when tterm--terminal
    (tterm--destroy (tterm-id tterm--terminal)))
  (tterm--dispose-terminal-buffer))

(defun tterm--command (id command &optional payload)
  "Send generic COMMAND with PAYLOAD to terminal ID."
  (tterm-bridge-command id command (or payload "")))

(defun tterm--decode-command-text (text)
  "Decode bridge command TEXT returned as UTF-8 bytes."
  (if (and (not (multibyte-string-p text))
           (string-match-p "[\200-\377]" text))
      (decode-coding-string text 'utf-8 t)
    text))

;;; Cross-pane attention

(declare-function tterm-dashboard--decode "tterm-dashboard" (text))
(declare-function tterm-dashboard-select-window
                  "tterm-dashboard" (handle &optional terminal-id))

(defun tterm--attention-window-unread-p (window)
  "Return non-nil when dashboard WINDOW has unread notifications."
  (> (or (plist-get window :unread-notifications) 0) 0))

(defun tterm--attention-windows-from-snapshot (snapshot)
  "Return unread dashboard windows from decoded SNAPSHOT."
  (let (windows)
    (dolist (group snapshot)
      (dolist (window (plist-get group :windows))
        (when (tterm--attention-window-unread-p window)
          (push window windows))))
    (nreverse windows)))

(defun tterm--attention-apply-snapshot (snapshot)
  "Update cached attention state from decoded dashboard SNAPSHOT."
  (setq tterm--attention-windows
        (tterm--attention-windows-from-snapshot snapshot))
  (setq tterm--attention-unread-total
        (apply #'+
               (mapcar (lambda (window)
                         (or (plist-get window :unread-notifications) 0))
                       tterm--attention-windows)))
  (force-mode-line-update t))

(defun tterm--attention-stop-timer ()
  "Stop the attention refresh timer and clear cached state."
  (when (timerp tterm--attention-refresh-timer)
    (cancel-timer tterm--attention-refresh-timer))
  (setq tterm--attention-refresh-timer nil)
  (setq tterm--attention-unread-total 0)
  (setq tterm--attention-windows nil)
  (force-mode-line-update t))

(defun tterm--attention-refresh ()
  "Refresh cached attention state from lightweight backend output."
  (if (null (tterm--buffers))
      (tterm--attention-stop-timer)
    (condition-case nil
        (progn
          (require 'tterm-dashboard)
          (tterm--attention-apply-snapshot
           (tterm-dashboard--decode (tterm-bridge-command 0 "attention" ""))))
      (error nil))))

(defun tterm--attention-ensure-timer ()
  "Ensure the attention refresh timer is running."
  (unless (timerp tterm--attention-refresh-timer)
    (setq tterm--attention-refresh-timer
          (run-at-time 0 tterm-attention-refresh-interval
                       #'tterm--attention-refresh))))

(defun tterm--attention-maybe-stop-later ()
  "Stop attention polling after the last tterm buffer is gone."
  (run-at-time 0 nil
               (lambda ()
                 (unless (tterm--buffers)
                   (tterm--attention-stop-timer)))))

(defun tterm--attention-setup-buffer ()
  "Set up global attention polling for a tterm buffer."
  (when (and (boundp 'tterm--terminal) tterm--terminal)
    (tterm--attention-ensure-timer))
  (add-hook 'kill-buffer-hook #'tterm--attention-maybe-stop-later nil t))

(defun tterm--attention-current-window-p (window)
  "Return non-nil when dashboard WINDOW is the current tterm buffer."
  (and (boundp 'tterm--terminal)
       tterm--terminal
       (or (and (plist-get window :terminal-id)
                (= (plist-get window :terminal-id)
                   (tterm-id tterm--terminal)))
           (and (tterm-handle tterm--terminal)
                (equal (plist-get window :handle)
                       (tterm-handle tterm--terminal))))))

(defun tterm--attention-target-window ()
  "Return the unread dashboard window to jump to."
  (or (cl-find-if-not #'tterm--attention-current-window-p
                      tterm--attention-windows)
      (car tterm--attention-windows)))

(defun tterm--attention-window-summary (window)
  "Return a concise display summary for unread dashboard WINDOW."
  (let ((name (or (plist-get window :name)
                  (plist-get window :window-id)
                  "tterm"))
        (notification (plist-get window :notification)))
    (if (and notification (not (string-empty-p notification)))
        (format "%s: %s" name notification)
      name)))

(defun tterm--mode-line-attention-indicator ()
  "Return mode-line text for global tterm unread attention."
  (if (<= tterm--attention-unread-total 0)
      ""
    (let ((text (format " [🔔:%d]" tterm--attention-unread-total)))
      (add-text-properties
       0 (length text)
       `(face tterm-notification-mode-line
         local-map ,tterm--attention-mode-line-map
         mouse-face mode-line-highlight
         help-echo "mouse-1 or C-c C-n: jump to tterm notification")
       text)
      text)))

(defun tterm--header-attention-indicator ()
  "Return header-line text for global tterm unread attention."
  (when (> tterm--attention-unread-total 0)
    (let ((summary (and tterm--attention-windows
                        (tterm--attention-window-summary
                         (car tterm--attention-windows)))))
      (if summary
          (format "attention: %s" summary)
        (format "attention: %d" tterm--attention-unread-total)))))

;;;###autoload
(defun tterm-jump-next-notification ()
  "Jump to the next tterm pane with unread terminal notifications."
  (interactive)
  (tterm--attention-refresh)
  (let ((window (tterm--attention-target-window)))
    (unless window
      (user-error "No unread tterm notifications"))
    (require 'tterm-dashboard)
    (tterm-dashboard-select-window
     (plist-get window :handle)
     (plist-get window :terminal-id))
    (run-at-time 0.5 nil #'tterm--attention-refresh)))

(defun tterm--escape-field (value)
  "Escape one tab-separated bridge field VALUE."
  (let ((index 0)
        (length (length value))
        (out nil))
    (while (< index length)
      (pcase (aref value index)
        (?\\ (push "\\\\" out))
        (?\t (push "\\t" out))
        (?\n (push "\\n" out))
        (char (push (string char) out)))
      (setq index (1+ index)))
    (apply #'concat (nreverse out))))

(defun tterm--unescape-field (value)
  "Decode one escaped bridge field VALUE."
  (let ((index 0)
        (length (length value))
        (out nil))
    (while (< index length)
      (let ((char (aref value index)))
        (if (and (= char ?\\) (< (1+ index) length))
            (let ((next (aref value (1+ index))))
              (push (pcase next
                      (?n ?\n)
                      (?t ?\t)
                      (?\\ ?\\)
                      (_ next))
                    out)
              (setq index (+ index 2)))
          (push char out)
          (setq index (1+ index)))))
    (apply #'string (nreverse out))))

(defun tterm--handle-key (key)
  "Return plist key for bridge handle KEY."
  (pcase key
    ("host" :host)
    ("namespace" :namespace)
    ("socket" :socket)
    ("session" :session)
    ("window" :window-id)
    ("pane" :pane-id)
    ("identity" :identity)
    (_ nil)))

(defun tterm--decode-handle-payload (text)
  "Decode bridge handle payload TEXT into a plist."
  (let (handle)
    (dolist (line (split-string (tterm--decode-command-text text) "\n" t))
      (pcase (mapcar #'tterm--unescape-field (split-string line "\t"))
        (`(,key ,value)
         (when-let* ((plist-key (tterm--handle-key key)))
           (setq handle (plist-put handle plist-key value))))))
    (when (and (plist-get handle :host)
               (plist-get handle :namespace)
               (plist-get handle :socket)
               (plist-get handle :session)
               (plist-get handle :window-id)
               (plist-get handle :pane-id))
      handle)))

(defun tterm--pending-handle-p (handle)
  "Return non-nil when HANDLE still names a pending tmux window."
  (or (string-prefix-p "@tterm-" (or (plist-get handle :window-id) ""))
      (string-prefix-p "%tterm-" (or (plist-get handle :pane-id) ""))))

(defun tterm--terminal-handle ()
  "Return the current terminal's stable tmux handle, or nil."
  (when tterm--terminal
    (let ((handle
           (condition-case nil
               (tterm--decode-handle-payload
                (tterm--command (tterm-id tterm--terminal)
                                "terminal-handle"
                                ""))
             (error nil))))
      (when (and handle (not (tterm--pending-handle-p handle)))
        handle))))

(defun tterm--encode-reattach-payload (handle rows cols)
  "Encode tmux HANDLE and terminal ROWS/COLS for reattach-window."
  (let ((lines
         (list (format "host\t%s"
                       (tterm--escape-field (plist-get handle :host)))
               (format "namespace\t%s"
                       (tterm--escape-field (plist-get handle :namespace)))
               (format "socket\t%s"
                       (tterm--escape-field (plist-get handle :socket)))
               (format "session\t%s"
                       (tterm--escape-field (plist-get handle :session)))
               (format "window\t%s"
                       (tterm--escape-field (plist-get handle :window-id)))
               (format "pane\t%s"
                       (tterm--escape-field (plist-get handle :pane-id)))
               (format "rows\t%d" rows)
               (format "cols\t%d" cols))))
    (when-let* ((identity (plist-get handle :identity)))
      (setq lines
            (append lines
                    (list (format "identity\t%s"
                                  (tterm--escape-field identity))))))
    (mapconcat #'identity lines "\n")))

(defun tterm--decode-reattach-response (text)
  "Decode reattach-window response TEXT."
  (let* ((text (tterm--decode-command-text text))
         (fields (mapcar #'tterm--unescape-field
                         (split-string text "\t"))))
    (pcase fields
      (`("A" ,id ,host ,cwd ,window-id ,pane-id ,name . ,rest)
       (list :id (string-to-number id)
             :host host
             :cwd cwd
             :window-id window-id
             :pane-id pane-id
             :name name
             :identity (car rest)))
      (`("E" ,message)
       (user-error "%s" message))
      (_
       (user-error "Invalid reattach-window response: %s" text)))))

(defun tterm--reattach-window (handle rows cols &optional no-select)
  "Reattach tmux HANDLE at ROWS/COLS and return the tterm buffer.
When NO-SELECT is non-nil, do not select the restored buffer."
  (unless (and (listp handle) (plist-member handle :window-id))
    (user-error "No stable tmux handle"))
  (let* ((payload (tterm--encode-reattach-payload handle rows cols))
         (response (tterm--decode-reattach-response
                    (tterm-bridge-command 0 "reattach-window" payload))))
    (tterm--attach-terminal-buffer
     (plist-get response :id)
     rows
     cols
     (plist-get response :host)
     (plist-get response :cwd)
     (plist-get response :window-id)
     (plist-get response :pane-id)
     (plist-get response :name)
     no-select
     (if (plist-get response :identity)
         (plist-put (copy-sequence handle) :identity
                    (plist-get response :identity))
       handle))))

(defun tterm--desktop-save-buffer (_desktop-dirname)
  "Return desktop metadata for the current tterm buffer."
  (when-let* ((handle (tterm--terminal-handle)))
    (list :tterm 1
          :handle handle
          :rows (tterm-rows tterm--terminal)
          :cols (tterm-cols tterm--terminal)
          :title (tterm-title tterm--terminal)
          :cwd (tterm-cwd tterm--terminal))))

(defun tterm--setup-desktop-save ()
  "Install tterm's desktop-save hook for the current buffer."
  (setq-local desktop-save-buffer #'tterm--desktop-save-buffer))

(defun tterm-restore-desktop-buffer (_file-name _buffer-name misc)
  "Restore a desktop-saved tterm buffer from MISC."
  (when (and (listp misc) (equal (plist-get misc :tterm) 1))
    (let ((handle (plist-get misc :handle))
          (rows (or (plist-get misc :rows) 24))
          (cols (or (plist-get misc :cols) 80)))
      (when handle
        (tterm--reattach-window handle rows cols t)))))

(defun tterm--register-desktop-handler ()
  "Register tterm's desktop restore handler."
  (when (boundp 'desktop-buffer-mode-handlers)
    (add-to-list 'desktop-buffer-mode-handlers
                 '(tterm-mode . tterm-restore-desktop-buffer))))

(with-eval-after-load 'desktop
  (tterm--register-desktop-handler))

(defun tterm--handle-exit (payload)
  "Handle terminal exit from PAYLOAD."
  (let ((status (if (> (length payload) 0)
                    (logior (ash (aref payload 0) 24) (ash (aref payload 1) 16)
                            (ash (aref payload 2) 8) (aref payload 3))
                  0)))
    (message "Terminal exited with status %d" status)))

(defun tterm-detach ()
  "Detach the current tterm buffer while preserving its tmux window."
  (interactive)
  (unless (eq major-mode 'tterm-mode)
    (user-error "Not in a tterm buffer"))
  (unless tterm--terminal
    (user-error "No terminal attached"))
  (tterm--detach-current-terminal)
  (kill-buffer (current-buffer)))

(defun tterm-kill-window ()
  "Kill the current tterm tmux window and close its Emacs buffer."
  (interactive)
  (unless (eq major-mode 'tterm-mode)
    (user-error "Not in a tterm buffer"))
  (unless tterm--terminal
    (user-error "No terminal attached"))
  (tterm--kill-current-terminal-window)
  (kill-buffer (current-buffer)))

(defun tterm-cleanup ()
  "Reset tterm's tmux runtime and close live tterm buffers."
  (interactive)
  (when (fboundp 'tterm-module--command)
    (condition-case err
        (tterm--command 0 "cleanup" "")
      (error
       (message "tterm cleanup bridge failed: %S" err))))
  (dolist (buffer (tterm--buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (tterm--dispose-terminal-buffer))
      (kill-buffer buffer)))
  (message "Cleaned up tterm tmux runtime"))

(defun tterm--shutdown-control-clients ()
  "Close tterm tmux control clients while preserving tmux windows."
  (when (fboundp 'tterm-module--command)
    (ignore-errors
      (tterm--command 0 "shutdown-clients" ""))))

(add-hook 'kill-emacs-hook #'tterm--shutdown-control-clients)

;;; Entry point

(require 'tterm-osc (expand-file-name "tterm-osc" tterm--directory))
(require 'tterm-apply (expand-file-name "tterm-apply" tterm--directory))
(require 'tterm-mode (expand-file-name "tterm-mode" tterm--directory))
(add-hook 'tterm-mode-hook #'tterm--setup-desktop-save)
(add-hook 'tterm-mode-hook #'tterm--attention-setup-buffer)

(defun tterm--attach-terminal-buffer
    (id rows cols host cwd &optional window-id pane-id title no-select handle)
  "Attach a tterm buffer to terminal ID using ROWS, COLS, HOST, and CWD.
When NO-SELECT is non-nil, do not select the buffer."
  (let* ((buf (or (tterm--buffer-for-terminal-id id)
                  (get-buffer-create (format "*tterm-%d*" id))))
         (term (make-tterm :id id
                            :buffer buf
                            :rows rows
                            :cols cols
                            :displayed-version 0
                            :title title
                            :cwd cwd
                            :host host
                            :window-id window-id
                            :pane-id pane-id
                            :handle handle)))
    (with-current-buffer buf
      (when (eq major-mode 'tterm-mode)
        (tterm--dispose-terminal-buffer))
      (tterm-mode)
      (setq-local tterm--terminal term)
      (setq-local tterm--title title)
      (setq-local default-directory cwd)
      (tterm--initialize-screen rows cols)
      (tterm--update-buffer-name)
      (tterm--attention-setup-buffer))
    (unless no-select
      (switch-to-buffer buf))
    (with-current-buffer buf
      (tterm--resize-window)
      (tterm--update-redraw-timer))
    buf))

(defun tterm--open-terminal-buffer
    (id rows cols host cwd &optional window-id pane-id title no-select handle)
  "Compatibility wrapper for `tterm--attach-terminal-buffer'."
  (tterm--attach-terminal-buffer id rows cols host cwd window-id pane-id title
                                 no-select handle))

;;;###autoload
(defun tterm (&optional remote)
  "Create a new tmux-backed tterm terminal.
With prefix REMOTE, prompt for an SSH host."
  (interactive "P")
  (let* ((host (if remote (tterm-read-remote-host) "local"))
         (grid (tterm--window-grid-size))
         (rows (car grid))
         (cols (cdr grid))
         (cwd (tterm--cwd-for-host host))
         (id (tterm--connect rows cols host
                             (tterm--normalize-start-cwd cwd))))
    (tterm--attach-terminal-buffer id rows cols host cwd)))

;;;###autoload
(autoload 'tterm-dashboard "tterm-dashboard" nil t)

(provide 'tterm)
;;; tterm.el ends here
