;;; markdown-adaptive-ts-mode.el --- Adaptive tables and HTML preview for markdown-ts-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Tung Dao

;; Author: Tung Dao <me@tungdao.com>
;; Keywords: text, markdown, tree-sitter, convenience, multimedia
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Major mode extending `markdown-ts-mode' with two essential features:
;;
;; 1. Adaptive Tables:
;;    Long Markdown tables in standard Emacs are cumbersome to read:
;;    - When `truncate-lines' is non-nil, wide cells trail off-screen to the right.
;;    - When `visual-line-mode' is on or `truncate-lines' is nil, single-line
;;      table rows wrap indiscriminately across cell boundaries, destroying the
;;      2D column structure and making readability far worse.
;;
;;    This mode provides an adaptive in-buffer table rendering engine:
;;    - Measures the available window/fill width and distributes column widths
;;      proportionally so the table fits comfortably without horizontal scrolling.
;;    - Gracefully wraps long text within each cell across multiple visual lines
;;      while strictly maintaining vertical column borders and alignment.
;;    - Displays tables using clean, modern box borders (rounded Unicode or ASCII).
;;    - Seamlessly reveals the raw Markdown table when point moves into it for
;;      editing, and re-renders the adaptive table when point moves away.
;;    - Includes interactive toggle commands (`markdown-adaptive-table-toggle').
;;
;; 2. HTML Rendered View (C-c C-c):
;;    Pressing `C-c C-c' renders the Markdown buffer into a clean, standalone,
;;    beautifully styled HTML document written directly to /tmp/<buffer-name>.html,
;;    and immediately opens it in the default browser.
;;    - Works out of the box with zero external dependencies (pure Elisp engine).
;;    - Also supports external converters (`pandoc', `cmark-gfm', `mdrender')
;;      via `markdown-adaptive-html-converter'.
;;    - Features a modern, responsive design with automatic dark/light mode,
;;      readable typography, styled responsive tables, and formatted code blocks.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'browse-url)
(require 'markdown-ts-mode)

(defgroup markdown-adaptive nil
  "Adaptive tables and HTML preview for `markdown-ts-mode'."
  :group 'markdown
  :prefix "markdown-adaptive-")

;;;; Customization Options

(defcustom markdown-adaptive-table-border-style 'rounded
  "Border style for adaptive tables.
Available styles:
  `rounded' - Unicode box drawing with rounded corners (╭─┬╮│├┼┤╰┴╯)
  `single'  - Unicode single-line box drawing (┌─┬┐│├┼┤└┴┘)
  `double'  - Unicode double-line box drawing (╔═╦╗║╠╬╣╚╩╝)
  `ascii'   - Plain ASCII characters (+-|)"
  :type '(choice (const :tag "Rounded Unicode" rounded)
                 (const :tag "Single Unicode" single)
                 (const :tag "Double Unicode" double)
                 (const :tag "ASCII" ascii))
  :group 'markdown-adaptive)

(defcustom markdown-adaptive-table-max-width nil
  "Maximum width in columns for adaptive tables.
If nil, adapt automatically to the current window body width minus margins."
  :type '(choice (const :tag "Fit window width" nil)
                 (integer :tag "Fixed column limit"))
  :group 'markdown-adaptive)

(defcustom markdown-adaptive-table-min-column-width 6
  "Minimum column width in characters for any column in an adaptive table."
  :type 'integer
  :group 'markdown-adaptive)

(defcustom markdown-adaptive-table-auto-reveal t
  "When non-nil, automatically reveal raw Markdown when point enters a table.
When point exits the table, it automatically re-renders as an adaptive table."
  :type 'boolean
  :group 'markdown-adaptive)

(defcustom markdown-adaptive-table-row-separator 'header-only
  "Separator style between rows in adaptive tables.
  `header-only' - Draw divider line only beneath header row.
  `all'         - Draw divider line between all table rows.
  `none'        - No horizontal divider lines inside the table body."
  :type '(choice (const :tag "Below header only" header-only)
                 (const :tag "Between every row" all)
                 (const :tag "None" none))
  :group 'markdown-adaptive)

(defcustom markdown-adaptive-html-output-dir "/tmp"
  "Directory where HTML preview files are written on C-c C-c."
  :type 'directory
  :group 'markdown-adaptive)

(defcustom markdown-adaptive-html-converter 'builtin
  "Converter to use for generating HTML preview.
  `builtin'  - Pure Elisp built-in Markdown-to-HTML engine (zero dependencies).
  `pandoc'   - Pandoc executable (`pandoc -f gfm -t html5 --standalone').
  `cmark'    - cmark or cmark-gfm executable.
  `mdrender' - Custom ~/Projects/dotfiles/bin/mdrender script.
  `auto'     - Use external tool if available, fallback to builtin."
  :type '(choice (const :tag "Built-in pure Elisp (zero dependencies)" builtin)
                 (const :tag "Auto (prefer external CLI, fallback to builtin)" auto)
                 (const :tag "Pandoc" pandoc)
                 (const :tag "cmark / cmark-gfm" cmark)
                 (const :tag "mdrender" mdrender))
  :group 'markdown-adaptive)

;;;; Faces

(defgroup markdown-adaptive-faces nil
  "Faces used by `markdown-adaptive-ts-mode'."
  :group 'markdown-adaptive
  :group 'faces)

(defface markdown-adaptive-table-border
  '((((class color) (background light)) (:inherit shadow :weight normal))
    (((class color) (background dark))  (:inherit shadow :weight normal))
    (t (:inherit shadow)))
  "Face used for drawing adaptive table box borders."
  :group 'markdown-adaptive-faces)

(defface markdown-adaptive-table-header
  '((((class color) (background light))
     (:inherit fixed-pitch :weight bold :foreground "#0969da"))
    (((class color) (background dark))
     (:inherit fixed-pitch :weight bold :foreground "#58a6ff"))
    (t (:inherit (bold fixed-pitch))))
  "Face used for column headers in adaptive tables."
  :group 'markdown-adaptive-faces)

(defface markdown-adaptive-table-cell
  '((t (:inherit fixed-pitch)))
  "Face used for cell text in adaptive tables."
  :group 'markdown-adaptive-faces)

;;;; Box Drawing Border Character Sets

(defconst markdown-adaptive--borders
  '((rounded . ((tl . "╭") (tm . "┬") (tr . "╮")
                (ml . "├") (mm . "┼") (mr . "┤")
                (bl . "╰") (bm . "┴") (br . "╯")
                (h  . "─") (v  . "│")))
    (single  . ((tl . "┌") (tm . "┬") (tr . "┐")
                (ml . "├") (mm . "┼") (mr . "┤")
                (bl . "└") (bm . "┴") (br . "┘")
                (h  . "─") (v  . "│")))
    (double  . ((tl . "╔") (tm . "╦") (tr . "╗")
                (ml . "╠") (mm . "╬") (mr . "╣")
                (bl . "╚") (bm . "╩") (br . "╝")
                (h  . "═") (v  . "║")))
    (ascii   . ((tl . "+") (tm . "+") (tr . "+")
                (ml . "+") (mm . "+") (mr . "+")
                (bl . "+") (bm . "+") (br . "+")
                (h  . "-") (v  . "|"))))
  "Alist mapping border style symbol to glyph definitions.")

(defvar markdown-adaptive-table-mode)

(defun markdown-adaptive--get-border-char (part)
  "Return border character for PART.
Uses `markdown-adaptive-table-border-style'."
  (let* ((style (or markdown-adaptive-table-border-style 'rounded))
         (set (or (alist-get style markdown-adaptive--borders)
                  (alist-get 'rounded markdown-adaptive--borders))))
    (or (alist-get part set) "+")))

;;;; Adaptive Table Parsing and Width Calculation

(defun markdown-adaptive--split-table-row (line)
  "Split Markdown table row LINE into a list of cell strings.
Correctly handles escaped pipes (\\|)."
  (let* ((trimmed (string-trim line))
         (inner (if (string-prefix-p "|" trimmed) (substring trimmed 1) trimmed))
         (inner (if (string-suffix-p "|" inner) (substring inner 0 -1) inner))
         (cells nil)
         (current "")
         (i 0)
         (len (length inner)))
    (while (< i len)
      (let ((ch (aref inner i)))
        (cond
         ((and (= ch ?\\) (< (1+ i) len) (= (aref inner (1+ i)) ?|))
          (setq current (concat current "|"))
          (setq i (1+ i)))
         ((= ch ?|)
          (push (string-trim current) cells)
          (setq current ""))
         (t
          (setq current (concat current (string ch))))))
      (setq i (1+ i)))
    (push (string-trim current) cells)
    (nreverse cells)))

(defun markdown-adaptive--parse-delimiter-cell (cell)
  "Determine column alignment (`left', `center', or `right') from delimiter CELL."
  (let ((s (string-trim cell)))
    (cond
     ((and (string-prefix-p ":" s) (string-suffix-p ":" s)) 'center)
     ((string-suffix-p ":" s) 'right)
     (t 'left))))

(defun markdown-adaptive--parse-table-region (beg end)
  "Parse Markdown table between BEG and END.
Returns a list (HEADERS ALIGNMENTS ROWS)."
  (let* ((text (buffer-substring-no-properties beg end))
         (lines (split-string text "\n" t))
         (header nil)
         (alignments nil)
         (rows nil))
    (when lines
      (setq header (markdown-adaptive--split-table-row (car lines)))
      (when (> (length lines) 1)
        (let ((delim-cells (markdown-adaptive--split-table-row (cadr lines))))
          (setq alignments (mapcar #'markdown-adaptive--parse-delimiter-cell delim-cells))))
      (dolist (line (cddr lines))
        (when (string-match-p "[|]" line)
          (push (markdown-adaptive--split-table-row line) rows))))
    ;; Ensure alignments has matching length
    (let ((ncols (length header)))
      (while (< (length alignments) ncols)
        (setq alignments (append alignments '(left)))))
    (list header alignments (nreverse rows))))

(defun markdown-adaptive--available-width ()
  "Compute the maximum available column width for adaptive tables."
  (let* ((win-width (if (window-live-p (selected-window))
                        (window-body-width (selected-window))
                      80))
         (max-target (or markdown-adaptive-table-max-width (max 40 (- win-width 4)))))
    (max 30 (min win-width max-target))))

(defun markdown-adaptive--calculate-column-widths (headers rows avail-width)
  "Calculate optimal adaptive widths for each column.
HEADERS is list of header strings; ROWS is list of row cell lists.
AVAIL-WIDTH is total character width available for table."
  (let* ((ncols (max (length headers) 1))
         ;; Overhead: 1 border left + 1 border right + (ncols - 1) internal borders
         ;; plus 2 padding spaces per column = 1 + 3 * ncols.
         (overhead (+ 1 (* 3 ncols)))
         (target-text-width (max (* ncols markdown-adaptive-table-min-column-width)
                                 (- avail-width overhead)))
         (natural-widths (make-vector ncols 0))
         (min-widths (make-vector ncols markdown-adaptive-table-min-column-width)))

    ;; 1. Measure natural maximum content width and minimum word width per column
    (dotimes (i ncols)
      (let ((h (or (nth i headers) "")))
        (aset natural-widths i (max (aref natural-widths i) (string-width h))))
      (dolist (row rows)
        (let* ((cell (or (nth i row) ""))
               (w (string-width cell)))
          (aset natural-widths i (max (aref natural-widths i) w))
          ;; Longest word should ideally fit without hyphenation
          (dolist (word (split-string cell "[ \t\n]+" t))
            (aset min-widths i (max (aref min-widths i) (min 20 (string-width word))))))))

    (let ((total-natural (cl-reduce #'+ natural-widths :initial-value 0)))
      (if (<= total-natural target-text-width)
          ;; Case A: Table fits comfortably within available width!
          (append natural-widths nil)
        ;; Case B: Table is wider than window: adaptively compress columns.
        (let* ((allocated (make-vector ncols 0))
               (remaining-width target-text-width))
          ;; First, assign each column its baseline width
          (dotimes (i ncols)
            (let ((base (min (aref natural-widths i)
                             (max markdown-adaptive-table-min-column-width
                                  (min 12 (aref min-widths i))))))
              (aset allocated i base)
              (setq remaining-width (- remaining-width base))))

          ;; If remaining width is positive, distribute proportionally to "hungry" columns
          (when (> remaining-width 0)
            (let* ((excess-needed
                    (cl-loop for i from 0 below ncols
                             sum (max 0 (- (aref natural-widths i) (aref allocated i)))))
                   (pool remaining-width))
              (if (> excess-needed 0)
                  (progn
                    (dotimes (i ncols)
                      (let* ((deficit (max 0 (- (aref natural-widths i) (aref allocated i))))
                             (share (floor (* pool (/ (float deficit) excess-needed)))))
                        (aset allocated i (+ (aref allocated i) share))
                        (setq remaining-width (- remaining-width share))))
                    ;; Distribute any rounding remainder
                    (let ((i 0))
                      (while (and (> remaining-width 0) (< i ncols))
                        (when (< (aref allocated i) (aref natural-widths i))
                          (aset allocated i (1+ (aref allocated i)))
                          (setq remaining-width (1- remaining-width)))
                        (setq i (1+ i)))))
                ;; If no excess needed, distribute remainder evenly
                (let ((i 0))
                  (while (> remaining-width 0)
                    (aset allocated (% i ncols) (1+ (aref allocated (% i ncols))))
                    (setq remaining-width (1- remaining-width))
                    (setq i (1+ i)))))))
          (append allocated nil))))))

;;;; Cell Wrapping and Text Formatting

(defun markdown-adaptive--wrap-cell-text (text width)
  "Wrap TEXT to fit within WIDTH characters without breaking words if possible."
  (if (or (null text) (string-empty-p text))
      '("")
    (if (<= (string-width text) width)
        (list text)
      (with-temp-buffer
        (insert text)
        (let ((fill-column (max 1 width)))
          (fill-region (point-min) (point-max)))
        (let ((lines (split-string (buffer-string) "\n" t)))
          (or lines '("")))))))

(defun markdown-adaptive--align-cell-line (text width align)
  "Format TEXT into a string of exact visual width WIDTH according to ALIGN.
ALIGN can be `left', `center', or `right'."
  (let* ((sw (string-width text))
         (pad (max 0 (- width sw))))
    (cond
     ((eq align 'right)
      (concat (make-string pad ?\s) text))
     ((eq align 'center)
      (let* ((left-pad (/ pad 2))
             (right-pad (- pad left-pad)))
        (concat (make-string left-pad ?\s) text (make-string right-pad ?\s))))
     (t ;; left
      (concat text (make-string pad ?\s))))))

;;;; Table Rendering into Formatted String

(defun markdown-adaptive--render-border-line (widths left-sym mid-sym right-sym horiz-sym)
  "Generate a horizontal border line with WIDTHS and border symbols."
  (let* ((b-face 'markdown-adaptive-table-border)
         (h-str (markdown-adaptive--get-border-char horiz-sym))
         (segments
          (mapcar (lambda (w)
                    (make-string (+ w 2) (string-to-char h-str)))
                  widths))
         (line (concat (markdown-adaptive--get-border-char left-sym)
                       (string-join segments (markdown-adaptive--get-border-char mid-sym))
                       (markdown-adaptive--get-border-char right-sym)
                       "\n")))
    (propertize line 'face b-face)))

(defun markdown-adaptive--format-table-string (headers alignments rows widths)
  "Format parsed table into an adaptive, multi-line string.
HEADERS: list of column title strings.
ALIGNMENTS: list of column alignments (`left', `center', `right').
ROWS: list of body row cell lists.
WIDTHS: list of column widths in characters."
  (let* ((v-char (markdown-adaptive--get-border-char 'v))
         (v-border (propertize v-char 'face 'markdown-adaptive-table-border))
         (ncols (length widths))
         (res ""))

    ;; 1. Top border
    (setq res (concat res (markdown-adaptive--render-border-line widths 'tl 'tm 'tr 'h)))

    ;; 2. Header row
    (let* ((wrapped-headers
            (cl-mapcar (lambda (h w)
                         (markdown-adaptive--wrap-cell-text (or h "") w))
                       headers widths))
           (h-height (apply #'max 1 (mapcar #'length wrapped-headers))))
      (dotimes (line-idx h-height)
        (let ((line-cells nil))
          (dotimes (col-idx ncols)
            (let* ((lines (nth col-idx wrapped-headers))
                   (cell-text (or (nth line-idx lines) ""))
                   (w (nth col-idx widths))
                   (align (or (nth col-idx alignments) 'left))
                   (padded (markdown-adaptive--align-cell-line cell-text w align)))
              (push (concat " " (propertize padded 'face 'markdown-adaptive-table-header) " ")
                    line-cells)))
          (setq res (concat res
                            v-border
                            (string-join (nreverse line-cells) v-border)
                            v-border
                            "\n")))))

    ;; 3. Delimiter / Header separator
    (setq res (concat res (markdown-adaptive--render-border-line widths 'ml 'mm 'mr 'h)))

    ;; 4. Body rows
    (let ((row-idx 0)
          (nrows (length rows)))
      (dolist (row rows)
        (let* ((wrapped-cells
                (cl-mapcar (lambda (col-idx w)
                             (let ((c (or (nth col-idx row) "")))
                               (markdown-adaptive--wrap-cell-text c w)))
                           (number-sequence 0 (1- ncols))
                           widths))
               (row-height (apply #'max 1 (mapcar #'length wrapped-cells))))
          (dotimes (line-idx row-height)
            (let ((line-cells nil))
              (dotimes (col-idx ncols)
                (let* ((lines (nth col-idx wrapped-cells))
                       (cell-text (or (nth line-idx lines) ""))
                       (w (nth col-idx widths))
                       (align (or (nth col-idx alignments) 'left))
                       (padded (markdown-adaptive--align-cell-line cell-text w align)))
                  (push (concat " " (propertize padded 'face 'markdown-adaptive-table-cell) " ")
                        line-cells)))
              (setq res (concat res
                                v-border
                                (string-join (nreverse line-cells) v-border)
                                v-border
                                "\n")))))
        (setq row-idx (1+ row-idx))
        ;; Optional separator between body rows
        (when (and (eq markdown-adaptive-table-row-separator 'all)
                   (< row-idx nrows))
          (setq res (concat res (markdown-adaptive--render-border-line widths 'ml 'mm 'mr 'h))))))

    ;; 5. Bottom border
    (setq res (concat res (markdown-adaptive--render-border-line widths 'bl 'bm 'br 'h)))
    res))

;;;; Overlay Management and Auto-Reveal Engine

(defvar-local markdown-adaptive--overlays nil
  "List of active adaptive table overlays in the current buffer.")

(defvar-local markdown-adaptive--active-table nil
  "Cons cell (BEG . END) of the table currently under cursor, or nil.")

(defun markdown-adaptive--table-at-pos (&optional pos)
  "Return (BEG . END) bounds of the table at POS, or nil if none."
  (let ((p (or pos (point))))
    (save-excursion
      (goto-char p)
      ;; Try tree-sitter first if available
      (if (and (fboundp 'markdown-ts-at-table-p)
               (derived-mode-p 'markdown-ts-mode))
          (when-let* ((at-table (markdown-ts-at-table-p p t))
                      (node (cdr at-table)))
            (cons (treesit-node-start node) (treesit-node-end node)))
        ;; Line-based fallback
        (let ((bol (line-beginning-position)))
          (when (save-excursion (goto-char bol) (looking-at-p "^[ \t]*|"))
            (let ((beg bol)
                  (end (line-end-position)))
              (save-excursion
                (while (and (> (point) (point-min))
                            (progn (forward-line -1)
                                   (looking-at-p "^[ \t]*|")))
                  (setq beg (line-beginning-position))))
              (save-excursion
                (goto-char bol)
                (while (and (< (point) (point-max))
                            (progn (forward-line 1)
                                   (looking-at-p "^[ \t]*|")))
                  (setq end (line-end-position))))
              (cons beg (min (point-max) (1+ end))))))))))

(defun markdown-adaptive--all-tables ()
  "Return list of all (BEG . END) table ranges in current buffer."
  (let ((ranges nil))
    (if (and (fboundp 'treesit-query-capture)
             (fboundp 'treesit-buffer-root-node)
             (treesit-ready-p 'markdown t))
        (when-let* ((root (treesit-buffer-root-node 'markdown))
                    (query (treesit-query-compile 'markdown '((pipe_table) @table)))
                    (captures (treesit-query-capture root query)))
          (dolist (cap captures)
            (let ((node (cdr cap)))
              (push (cons (treesit-node-start node) (treesit-node-end node)) ranges))))
      ;; Regex fallback
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*|.*|[ \t]*$" nil t)
          (let ((bounds (markdown-adaptive--table-at-pos (point))))
            (when bounds
              (push bounds ranges)
              (goto-char (cdr bounds)))))))
    (nreverse ranges)))

(defun markdown-adaptive--remove-overlay-for-table (beg end)
  "Remove any adaptive overlay covering BEG to END."
  (setq markdown-adaptive--overlays
        (cl-remove-if
         (lambda (ov)
           (when (and (overlay-buffer ov)
                      (<= (overlay-start ov) end)
                      (>= (overlay-end ov) beg))
             (delete-overlay ov)
             t))
         markdown-adaptive--overlays)))

(defun markdown-adaptive--render-table-range (beg end)
  "Render adaptive table overlay for range BEG to END."
  (markdown-adaptive--remove-overlay-for-table beg end)
  (let* ((parsed (markdown-adaptive--parse-table-region beg end))
         (headers (nth 0 parsed))
         (alignments (nth 1 parsed))
         (rows (nth 2 parsed)))
    (when (and headers (> (length headers) 0))
      (let* ((avail-w (markdown-adaptive--available-width))
             (widths (markdown-adaptive--calculate-column-widths headers rows avail-w))
             (rendered (markdown-adaptive--format-table-string headers alignments rows widths))
             (ov (make-overlay beg end nil t t)))
        (overlay-put ov 'markdown-adaptive-table t)
        (overlay-put ov 'display rendered)
        (overlay-put ov 'priority 15)
        (overlay-put ov 'evaporate t)
        (push ov markdown-adaptive--overlays)
        ov))))

(defun markdown-adaptive-table-render-all ()
  "Render all tables in current buffer with adaptive layouts."
  (interactive)
  (let ((cur-table (and markdown-adaptive-table-auto-reveal
                        (markdown-adaptive--table-at-pos (point)))))
    (dolist (range (markdown-adaptive--all-tables))
      (let ((tbeg (car range))
            (tend (cdr range)))
        ;; Skip table where point currently is so user can edit
        (unless (and cur-table
                     (<= tbeg (point))
                     (<= (point) tend))
          (markdown-adaptive--render-table-range tbeg tend))))))

(defun markdown-adaptive-table-clear-all ()
  "Remove all adaptive table overlays in current buffer."
  (interactive)
  (dolist (ov markdown-adaptive--overlays)
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq markdown-adaptive--overlays nil)
  (setq markdown-adaptive--active-table nil))

(defun markdown-adaptive-table-toggle (&optional arg)
  "Toggle adaptive table rendering at point, or for whole buffer with prefix ARG."
  (interactive "P")
  (if arg
      (if markdown-adaptive--overlays
          (progn
            (markdown-adaptive-table-clear-all)
            (message "Adaptive tables cleared for buffer."))
        (markdown-adaptive-table-render-all)
        (message "Adaptive tables rendered for buffer."))
    ;; Single table toggle
    (if-let* ((bounds (markdown-adaptive--table-at-pos (point))))
        (let* ((tbeg (car bounds))
               (tend (cdr bounds))
               (ov (cl-find-if (lambda (o)
                                 (and (overlay-buffer o)
                                      (= (overlay-start o) tbeg)
                                      (= (overlay-end o) tend)))
                               markdown-adaptive--overlays)))
          (if ov
              (progn
                (markdown-adaptive--remove-overlay-for-table tbeg tend)
                (message "Table revealed."))
            (markdown-adaptive--render-table-range tbeg tend)
            (message "Table adaptively rendered.")))
      (user-error "Point is not inside a Markdown table"))))

(defun markdown-adaptive--post-command-hook ()
  "Hook run after commands to manage auto-reveal of tables under cursor."
  (when markdown-adaptive-table-auto-reveal
    (let ((current-bounds (markdown-adaptive--table-at-pos (point))))
      (cond
       ;; Case 1: Point moved INTO a table
       ((and current-bounds
             (not (equal current-bounds markdown-adaptive--active-table)))
        ;; If there was an active table before, re-render it
        (when (and markdown-adaptive--active-table
                   (not (equal current-bounds markdown-adaptive--active-table)))
          (markdown-adaptive--render-table-range (car markdown-adaptive--active-table)
                                                 (cdr markdown-adaptive--active-table)))
        ;; Reveal current table for editing
        (markdown-adaptive--remove-overlay-for-table (car current-bounds) (cdr current-bounds))
        (setq markdown-adaptive--active-table current-bounds))

       ;; Case 2: Point moved OUT of a table
       ((and (null current-bounds)
             markdown-adaptive--active-table)
        (markdown-adaptive--render-table-range (car markdown-adaptive--active-table)
                                               (cdr markdown-adaptive--active-table))
        (setq markdown-adaptive--active-table nil))))))

(defun markdown-adaptive--window-size-change (&rest _)
  "Re-render adaptive tables when window width changes."
  (when markdown-adaptive-table-mode
    (markdown-adaptive-table-render-all)))

;;;###autoload
(define-minor-mode markdown-adaptive-table-mode
  "Minor mode providing adaptive, multi-line table rendering in Markdown.
Long cells are word-wrapped into multiple visual lines while strictly
preserving column alignment and borders. Tables automatically reveal
for raw editing when point enters, and re-render when point departs."
  :lighter " [adapt-tbl]"
  :group 'markdown-adaptive
  (if markdown-adaptive-table-mode
      (progn
        (add-hook 'post-command-hook #'markdown-adaptive--post-command-hook nil t)
        (add-hook 'window-size-change-functions #'markdown-adaptive--window-size-change nil t)
        (markdown-adaptive-table-render-all))
    (remove-hook 'post-command-hook #'markdown-adaptive--post-command-hook t)
    (remove-hook 'window-size-change-functions #'markdown-adaptive--window-size-change t)
    (markdown-adaptive-table-clear-all)))

;;;; HTML Rendering and Browser Preview Engine (C-c C-c)

(defun markdown-adaptive--escape-html (str)
  "Escape &, <, >, and \" characters in STR."
  (let ((res (replace-regexp-in-string "&" "&amp;" str t t)))
    (setq res (replace-regexp-in-string "<" "&lt;" res t t))
    (setq res (replace-regexp-in-string ">" "&gt;" res t t))
    (setq res (replace-regexp-in-string "\"" "&quot;" res t t))
    res))

(defun markdown-adaptive--render-inline-html (text)
  "Convert inline Markdown formatting in TEXT to HTML."
  (let ((codes nil)
        (idx 0)
        (res text))
    ;; 1. Extract inline code spans
    (setq res (replace-regexp-in-string
               "`\\([^`\n]+\\)`"
               (lambda (m)
                 (let ((code (match-string 1 m))
                       (ph (format "\x00CODE%d\x00" idx)))
                   (push (cons ph (format "<code>%s</code>"
                                          (markdown-adaptive--escape-html code)))
                         codes)
                   (cl-incf idx)
                   ph))
               res t t))
    ;; 2. Escape HTML
    (setq res (markdown-adaptive--escape-html res))
    ;; 3. Images: ![alt](url)
    (setq res (replace-regexp-in-string
               "!\\[\\([^]]*\\)\\](\\([^)]+\\))"
               "<img src=\"\\2\" alt=\"\\1\" loading=\"lazy\">"
               res t))
    ;; 4. Links: [text](url)
    (setq res (replace-regexp-in-string
               "\\[\\([^]]+\\)\\](\\([^)]+\\))"
               "<a href=\"\\2\" target=\"_blank\" rel=\"noopener noreferrer\">\\1</a>"
               res t))
    ;; 5. Bold: **text** or __text__
    (setq res (replace-regexp-in-string "\\*\\*\\([^*]+\\)\\*\\*" "<strong>\\1</strong>" res t))
    (setq res (replace-regexp-in-string "__\\([^_]+\\)__" "<strong>\\1</strong>" res t))
    ;; 6. Italic: *text* or _text_
    (setq res (replace-regexp-in-string "\\*\\([^*]+\\)\\*" "<em>\\1</em>" res t))
    (setq res (replace-regexp-in-string "_\\([^_]+\\)_" "<em>\\1</em>" res t))
    ;; 7. Strikethrough: ~~text~~
    (setq res (replace-regexp-in-string "~~\\([^~]+\\)~~" "<del>\\1</del>" res t))
    ;; 8. Restore code spans
    (dolist (c codes)
      (setq res (replace-regexp-in-string (regexp-quote (car c)) (cdr c) res t t)))
    res))

(defun markdown-adaptive--table-data-to-html (table-data)
  "Convert TABLE-DATA (HEADERS ALIGNMENTS ROWS) into HTML table."
  (let* ((headers (nth 0 table-data))
         (aligns (nth 1 table-data))
         (rows (nth 2 table-data))
         (out "<div class=\"table-container\"><table>\n"))
    (when headers
      (setq out (concat out "  <thead>\n    <tr>\n"))
      (dotimes (i (length headers))
        (let ((align (or (nth i aligns) 'left))
              (th-text (markdown-adaptive--render-inline-html (nth i headers))))
          (setq out (concat out (format "      <th style=\"text-align: %s\">%s</th>\n"
                                        align th-text)))))
      (setq out (concat out "    </tr>\n  </thead>\n")))
    (when rows
      (setq out (concat out "  <tbody>\n"))
      (dolist (row rows)
        (setq out (concat out "    <tr>\n"))
        (dotimes (i (max (length headers) (length row)))
          (let ((align (or (nth i aligns) 'left))
                (td-text (markdown-adaptive--render-inline-html (or (nth i row) ""))))
            (setq out (concat out (format "      <td style=\"text-align: %s\">%s</td>\n"
                                          align td-text)))))
        (setq out (concat out "    </tr>\n")))
      (setq out (concat out "  </tbody>\n")))
    (concat out "</table></div>\n")))

(defun markdown-adaptive--markdown-to-html-body (content)
  "Convert Markdown CONTENT string into HTML body."
  (let* ((lines (split-string content "\n"))
         (out "")
         (state nil)
         (block-buf nil)
         (lang nil)
         (in-list nil))
    (dolist (line lines)
      (cond
       ;; In code block
       ((eq state 'code)
        (if (string-match-p "^```[ \t]*$" line)
            (progn
              (let* ((code-raw (string-join (nreverse block-buf) "\n"))
                     (code-esc (markdown-adaptive--escape-html code-raw))
                     (lang-badge (if lang (format "<div class=\"code-header\"><span class=\"code-lang\">%s</span></div>" (markdown-adaptive--escape-html lang)) "")))
                (setq out (concat out (format "<div class=\"code-block\">%s<pre><code>%s</code></pre></div>\n"
                                              lang-badge code-esc))))
              (setq state nil block-buf nil lang nil))
          (push line block-buf)))

       ;; In table
       ((eq state 'table)
        (if (string-match-p "^[ \t]*|.*|[ \t]*$" line)
            (push line block-buf)
          ;; Table finished: render it
          (let ((table-data (markdown-adaptive--parse-table-lines (nreverse block-buf))))
            (setq out (concat out (markdown-adaptive--table-data-to-html table-data))))
          (setq state nil block-buf nil)
          ;; Process current line outside table
          (unless (string-blank-p line)
            (if in-list (progn (setq out (concat out "</ul>\n")) (setq in-list nil)))
            (setq out (concat out (format "<p>%s</p>\n" (markdown-adaptive--render-inline-html line)))))))

       ;; Code fence start
       ((string-match "^```\\([a-zA-Z0-9_-]*\\)" line)
        (if in-list (progn (setq out (concat out "</ul>\n")) (setq in-list nil)))
        (setq state 'code
              lang (let ((l (match-string 1 line))) (if (string-empty-p l) nil l))
              block-buf nil))

       ;; Table row start
       ((string-match-p "^[ \t]*|.*|[ \t]*$" line)
        (if in-list (progn (setq out (concat out "</ul>\n")) (setq in-list nil)))
        (setq state 'table block-buf (list line)))

       ;; ATX Heading (# Heading)
       ((and (string-match "^\\(#+\\)[ \t]+\\(.*\\)$" line)
             (<= (length (match-string 1 line)) 6))
        (if in-list (progn (setq out (concat out "</ul>\n")) (setq in-list nil)))
        (let* ((level (length (match-string 1 line)))
               (heading-text (markdown-adaptive--render-inline-html (match-string 2 line)))
               (slug (downcase (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" (match-string 2 line)))))
          (setq out (concat out (format "<h%d id=\"%s\">%s</h%d>\n" level slug heading-text level)))))

       ;; Blockquote (> quote)
       ((string-match "^>[ \t]?\\(.*\\)$" line)
        (if in-list (progn (setq out (concat out "</ul>\n")) (setq in-list nil)))
        (let ((q-text (markdown-adaptive--render-inline-html (match-string 1 line))))
          (setq out (concat out (format "<blockquote><p>%s</p></blockquote>\n" q-text)))))

       ;; Thematic break (---, ***)
       ((string-match-p "^[ \t]*\\(---+\\|\\*\\*\\*+\\|___+\\)[ \t]*$" line)
        (if in-list (progn (setq out (concat out "</ul>\n")) (setq in-list nil)))
        (setq out (concat out "<hr>\n")))

       ;; Task list item (- [ ] or - [x])
       ((string-match "^[ \t]*[-*+][ \t]+\\[\\([ xX]\\)\\][ \t]+\\(.*\\)$" line)
        (unless in-list
          (setq out (concat out "<ul class=\"task-list\">\n"))
          (setq in-list t))
        (let* ((checked (string-match-p "[xX]" (match-string 1 line)))
               (chk-attr (if checked "checked=\"checked\"" ""))
               (item-text (markdown-adaptive--render-inline-html (match-string 2 line))))
          (setq out (concat out (format "<li class=\"task-list-item\"><input type=\"checkbox\" %s disabled=\"disabled\"> %s</li>\n"
                                        chk-attr item-text)))))

       ;; Regular list item (- item)
       ((string-match "^[ \t]*[-*+][ \t]+\\(.*\\)$" line)
        (unless in-list
          (setq out (concat out "<ul>\n"))
          (setq in-list t))
        (let ((item-text (markdown-adaptive--render-inline-html (match-string 1 line))))
          (setq out (concat out (format "<li>%s</li>\n" item-text)))))

       ;; Blank line
       ((string-blank-p line)
        (when in-list
          (setq out (concat out "</ul>\n"))
          (setq in-list nil)))

       ;; Paragraph
       (t
        (if in-list (progn (setq out (concat out "</ul>\n")) (setq in-list nil)))
        (setq out (concat out (format "<p>%s</p>\n" (markdown-adaptive--render-inline-html line)))))))

    ;; Clean up any open states
    (when (eq state 'table)
      (let ((table-data (markdown-adaptive--parse-table-lines (nreverse block-buf))))
        (setq out (concat out (markdown-adaptive--table-data-to-html table-data)))))
    (when (eq state 'code)
      (let ((code-raw (string-join (nreverse block-buf) "\n")))
        (setq out (concat out (format "<div class=\"code-block\"><pre><code>%s</code></pre></div>\n"
                                      (markdown-adaptive--escape-html code-raw))))))
    (when in-list
      (setq out (concat out "</ul>\n")))
    out))

(defun markdown-adaptive--parse-table-lines (lines)
  "Parse a list of table row LINES into (HEADERS ALIGNMENTS ROWS)."
  (let ((header nil)
        (aligns nil)
        (rows nil))
    (when lines
      (setq header (markdown-adaptive--split-table-row (car lines)))
      (when (> (length lines) 1)
        (let ((delim-cells (markdown-adaptive--split-table-row (cadr lines))))
          (setq aligns (mapcar #'markdown-adaptive--parse-delimiter-cell delim-cells))))
      (dolist (line (cddr lines))
        (push (markdown-adaptive--split-table-row line) rows)))
    (list header aligns (nreverse rows))))

(defun markdown-adaptive--extract-headings (content)
  "Extract headings ((LEVEL TITLE SLUG) ...) from Markdown CONTENT."
  (let ((lines (split-string content "\n"))
        (headings nil)
        (in-code nil))
    (dolist (line lines)
      (cond
       ((string-match-p "^```" line)
        (setq in-code (not in-code)))
       ((and (not in-code)
             (string-match "^\\(#+\\)[ \t]+\\(.*\\)$" line)
             (<= (length (match-string 1 line)) 6))
        (let* ((lvl (length (match-string 1 line)))
               (raw-txt (string-trim (match-string 2 line)))
               (slug (downcase (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" raw-txt))))
          (push (list lvl raw-txt slug) headings)))))
    (nreverse headings)))

(defun markdown-adaptive--build-toc (headings)
  "Build sidebar TOC HTML from HEADINGS list."
  (if (null headings)
      ""
    (concat
     "<aside class=\"toc-sidebar\" id=\"toc-sidebar\">\n"
     "  <div class=\"toc-sticky-box\">\n"
     "    <div class=\"toc-header\">\n"
     "      <span class=\"toc-title\">Contents</span>\n"
     "    </div>\n"
     "    <nav class=\"toc-nav\">\n"
     "      <ul class=\"toc-list\">\n"
     (mapconcat
      (lambda (h)
        (let ((lvl (nth 0 h))
              (txt (markdown-adaptive--escape-html (nth 1 h)))
              (slug (nth 2 h)))
          (format "        <li class=\"toc-item toc-level-%d\"><a href=\"#%s\">%s</a></li>"
                  lvl slug txt)))
      headings "\n")
     "\n      </ul>\n"
     "    </nav>\n"
     "  </div>\n"
     "</aside>\n")))

(defconst markdown-adaptive--html-template
  "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  <title>{{TITLE}}</title>
  <style>
    :root {
      --font-body: -apple-system, BlinkMacSystemFont, \"Segoe UI\", \"Inter\", Roboto, \"Helvetica Neue\", system-ui, sans-serif;
      --font-mono: \"JetBrains Mono\", \"SF Mono\", ui-monospace, Menlo, Monaco, Consolas, monospace;
      --bg: #ffffff;
      --fg: #24292f;
      --muted: #57606a;
      --border: #d0d7de;
      --border-subtle: #eaeef2;
      --bg-subtle: #f6f8fa;
      --accent: #0969da;
      --accent-subtle: #ddf4ff;
      --code-bg: #f6f8fa;
      --table-stripe: #fbfcfd;
      --table-hover: #f3f4f6;
      --shadow: 0 1px 3px rgba(0, 0, 0, 0.06);
    }
    @media (prefers-color-scheme: dark) {
      :root {
        --bg: #0d1117;
        --fg: #e6edf3;
        --muted: #8b949e;
        --border: #30363d;
        --border-subtle: #21262d;
        --bg-subtle: #161b22;
        --accent: #58a6ff;
        --accent-subtle: #033875;
        --code-bg: #161b22;
        --table-stripe: #12161d;
        --table-hover: #1c2128;
        --shadow: 0 1px 3px rgba(0, 0, 0, 0.35);
      }
    }
    *, *::before, *::after { box-sizing: border-box; }
    html {
      scroll-behavior: smooth;
    }
    body {
      margin: 0;
      padding: 0;
      font-family: var(--font-body);
      font-size: 18px;
      line-height: 1.75;
      letter-spacing: -0.011em;
      color: var(--fg);
      background-color: var(--bg);
      text-rendering: optimizeLegibility;
      -webkit-font-smoothing: antialiased;
    }
    .page-layout {
      display: flex;
      justify-content: center;
      max-width: 1260px;
      margin: 0 auto;
      padding: 44px 32px;
      gap: 52px;
    }
    .page-layout.no-toc .toc-sidebar {
      display: none;
    }
    .page-layout.no-toc .main-content-wrapper {
      max-width: 860px;
      margin: 0 auto;
    }
    /* Sticky Side TOC Navigation */
    .toc-sidebar {
      width: 250px;
      flex-shrink: 0;
    }
    .toc-sticky-box {
      position: sticky;
      top: 36px;
      max-height: calc(100vh - 72px);
      overflow-y: auto;
      padding-right: 12px;
    }
    .toc-header {
      font-size: 12px;
      font-weight: 700;
      text-transform: uppercase;
      letter-spacing: 0.8px;
      color: var(--muted);
      margin-bottom: 12px;
      padding-bottom: 8px;
      border-bottom: 1px solid var(--border-subtle);
    }
    .toc-list {
      list-style: none;
      padding-left: 0;
      margin: 0;
    }
    .toc-item {
      margin: 3px 0;
      font-size: 14.5px;
      line-height: 1.45;
    }
    .toc-item a {
      display: block;
      padding: 6px 10px;
      border-radius: 6px;
      color: var(--muted);
      text-decoration: none;
      transition: all 0.15s ease;
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
    }
    .toc-item a:hover {
      color: var(--fg);
      background-color: var(--bg-subtle);
    }
    .toc-item.active a {
      color: var(--accent);
      font-weight: 600;
      background-color: var(--accent-subtle);
    }
    .toc-level-1 { font-weight: 600; }
    .toc-level-2 { padding-left: 12px; }
    .toc-level-3 { padding-left: 24px; font-size: 13.5px; }
    .toc-level-4 { padding-left: 36px; font-size: 13px; }

    /* Main Content */
    .main-content-wrapper {
      flex: 1;
      min-width: 0;
      max-width: 860px;
    }
    header.doc-header {
      margin-bottom: 36px;
      padding-bottom: 18px;
      border-bottom: 1px solid var(--border);
      display: flex;
      justify-content: space-between;
      align-items: center;
      font-size: 13.5px;
      color: var(--muted);
    }
    .doc-title { font-weight: 600; }
    h1, h2, h3, h4, h5, h6 {
      color: var(--fg);
      font-weight: 600;
      line-height: 1.3;
      scroll-margin-top: 36px;
    }
    h1 {
      font-size: 2.35rem;
      font-weight: 700;
      letter-spacing: -0.025em;
      margin-top: 1.6em;
      margin-bottom: 0.6em;
      padding-bottom: 0.35em;
      border-bottom: 1px solid var(--border);
    }
    h2 {
      font-size: 1.7rem;
      letter-spacing: -0.02em;
      margin-top: 1.6em;
      margin-bottom: 0.5em;
      padding-bottom: 0.3em;
      border-bottom: 1px solid var(--border-subtle);
    }
    h3 {
      font-size: 1.35rem;
      letter-spacing: -0.015em;
      margin-top: 1.4em;
      margin-bottom: 0.4em;
    }
    h4 {
      font-size: 1.15rem;
      margin-top: 1.2em;
      margin-bottom: 0.4em;
    }
    p { margin: 1.25em 0; }
    a { color: var(--accent); text-decoration: none; }
    a:hover { text-decoration: underline; }
    /* Responsive Adaptive Tables */
    .table-container {
      margin: 28px 0;
      overflow-x: auto;
      border: 1px solid var(--border);
      border-radius: 8px;
      box-shadow: var(--shadow);
      background: var(--bg);
    }
    table {
      width: 100%;
      border-collapse: collapse;
      font-size: 15.5px;
      line-height: 1.6;
    }
    th {
      background: var(--bg-subtle);
      font-weight: 600;
      padding: 12px 18px;
      border-bottom: 2px solid var(--border);
      white-space: nowrap;
    }
    td {
      padding: 12px 18px;
      border-bottom: 1px solid var(--border-subtle);
      vertical-align: top;
    }
    tr:last-child td { border-bottom: none; }
    tr:nth-child(even) td { background-color: var(--table-stripe); }
    tr:hover td { background-color: var(--table-hover); }
    /* Code Blocks */
    .code-block {
      margin: 24px 0;
      border: 1px solid var(--border);
      border-radius: 8px;
      overflow: hidden;
      background: var(--code-bg);
      box-shadow: var(--shadow);
    }
    .code-header {
      padding: 8px 16px;
      font-family: var(--font-mono);
      font-size: 12.5px;
      color: var(--muted);
      background: var(--bg-subtle);
      border-bottom: 1px solid var(--border);
      text-transform: uppercase;
      letter-spacing: 0.5px;
    }
    pre {
      margin: 0;
      padding: 18px 20px;
      overflow-x: auto;
      font-family: var(--font-mono);
      font-size: 15px;
      line-height: 1.55;
    }
    code {
      font-family: var(--font-mono);
      font-size: 0.9em;
      padding: 3px 7px;
      border-radius: 5px;
      background: var(--code-bg);
      border: 1px solid var(--border-subtle);
    }
    pre code {
      padding: 0;
      background: none;
      border: none;
      font-size: inherit;
    }
    /* Blockquotes */
    blockquote {
      margin: 24px 0;
      padding: 10px 22px;
      border-left: 4px solid var(--accent);
      background: var(--bg-subtle);
      border-radius: 0 8px 8px 0;
      color: var(--muted);
      font-size: 1.02em;
    }
    blockquote p { margin: 6px 0; }
    /* Lists & Task Lists */
    ul, ol { padding-left: 32px; margin: 18px 0; }
    li { margin: 8px 0; }
    ul.task-list { list-style: none; padding-left: 0; }
    .task-list-item { display: flex; align-items: baseline; gap: 10px; }
    .task-list-item input[type=\"checkbox\"] { accent-color: var(--accent); width: 16px; height: 16px; }
    hr {
      height: 1px;
      background-color: var(--border);
      border: none;
      margin: 36px 0;
    }
    img { max-width: 100%; height: auto; border-radius: 8px; }
    footer.doc-footer {
      margin-top: 56px;
      padding-top: 24px;
      border-top: 1px solid var(--border);
      font-size: 13px;
      color: var(--muted);
      text-align: center;
    }
    @media (max-width: 980px) {
      .page-layout {
        flex-direction: column;
        padding: 24px 18px;
        gap: 28px;
      }
      .toc-sidebar {
        width: 100%;
      }
      .toc-sticky-box {
        position: static;
        max-height: none;
      }
    }
  </style>
</head>
<body>
  <div class=\"page-layout {{HAS_TOC_CLASS}}\">
{{TOC}}
    <div class=\"main-content-wrapper\">
      <header class=\"doc-header\">
        <span class=\"doc-title\">{{TITLE}}</span>
        <span class=\"doc-meta\">Rendered: {{RENDER_TIME}}</span>
      </header>
      <main class=\"markdown-body\">
{{BODY}}
      </main>
      <footer class=\"doc-footer\">
        Rendered with markdown-adaptive-ts-mode (Emacs)
      </footer>
    </div>
  </div>
  <script>
    document.addEventListener('DOMContentLoaded', () => {
      const links = document.querySelectorAll('.toc-item a');
      const headings = document.querySelectorAll('h1[id], h2[id], h3[id], h4[id]');
      if (!links.length || !headings.length) return;
      const observer = new IntersectionObserver((entries) => {
        entries.forEach(entry => {
          if (entry.isIntersecting) {
            const id = entry.target.getAttribute('id');
            links.forEach(link => {
              const li = link.closest('.toc-item');
              if (link.getAttribute('href') === '#' + id) {
                li.classList.add('active');
              } else {
                li.classList.remove('active');
              }
            });
          }
        });
      }, { rootMargin: '0px 0px -75% 0px' });
      headings.forEach(h => observer.observe(h));
    });
  </script>
</body>
</html>")

(defun markdown-adaptive--render-html (title content &optional headings)
  "Wrap body CONTENT with full HTML template using TITLE and HEADINGS."
  (let* ((time-str (format-time-string "%Y-%m-%d %H:%M:%S"))
         (title-esc (markdown-adaptive--escape-html title))
         (hdgs (or headings (markdown-adaptive--extract-headings content)))
         (toc-html (markdown-adaptive--build-toc hdgs))
         (has-toc (if (and hdgs (> (length hdgs) 0)) "has-toc" "no-toc"))
         (res markdown-adaptive--html-template))
    (setq res (replace-regexp-in-string "{{TITLE}}" title-esc res t t))
    (setq res (replace-regexp-in-string "{{RENDER_TIME}}" time-str res t t))
    (setq res (replace-regexp-in-string "{{HAS_TOC_CLASS}}" has-toc res t t))
    (setq res (replace-regexp-in-string "{{TOC}}" (lambda (_) toc-html) res t t))
    (setq res (replace-regexp-in-string "{{BODY}}" (lambda (_) content) res t t))
    res))

;;;###autoload
(defun markdown-adaptive-preview-html (&optional arg)
  "Render current Markdown buffer to HTML in /tmp and open in default browser.
With prefix ARG, prompt for output destination."
  (interactive "P")
  ;; Save modified buffer if visiting file
  (when (and buffer-file-name (buffer-modified-p))
    (save-buffer))
  (let* ((base-name (if buffer-file-name
                        (file-name-base buffer-file-name)
                      (string-trim (buffer-name) "[*]" "[*]")))
         (title (if buffer-file-name (file-name-nondirectory buffer-file-name) base-name))
         (output-file
          (if arg
              (read-file-name "Write HTML to: " markdown-adaptive-html-output-dir nil nil
                              (format "%s.html" base-name))
            (expand-file-name (format "%s.html" base-name) markdown-adaptive-html-output-dir)))
         (content (buffer-substring-no-properties (point-min) (point-max))))

    (make-directory (file-name-directory output-file) t)

    ;; Render using configured converter
    (cond
     ((and (eq markdown-adaptive-html-converter 'pandoc) (executable-find "pandoc"))
      (call-process-region (point-min) (point-max) "pandoc" nil `(:file ,output-file) nil
                           "-f" "gfm" "-t" "html5" "--standalone"
                           "--metadata" (format "title=%s" title)))
     ((and (eq markdown-adaptive-html-converter 'cmark) (executable-find "cmark-gfm"))
      (call-process-region (point-min) (point-max) "cmark-gfm" nil `(:file ,output-file) nil
                           "--extension" "table" "--extension" "tasklist" "--extension" "strikethrough"))
     ((and (eq markdown-adaptive-html-converter 'mdrender)
           (file-executable-p (expand-file-name "~/Projects/dotfiles/bin/mdrender")))
      (call-process (expand-file-name "~/Projects/dotfiles/bin/mdrender") nil nil nil
                    (or buffer-file-name (make-temp-file "md-")) output-file))
     (t
      ;; Default built-in pure Elisp converter (zero dependencies)
      (let* ((html-body (markdown-adaptive--markdown-to-html-body content))
             (headings (markdown-adaptive--extract-headings content))
             (full-html (markdown-adaptive--render-html title html-body headings)))
        (with-temp-file output-file
          (insert full-html)))))

    ;; Open rendered HTML in default browser
    (browse-url-of-file output-file)
    (message "HTML preview written to %s and opened in browser." output-file)
    output-file))

;;;; Major Mode Definition

(defvar markdown-adaptive-ts-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Primary user requested binding: C-c C-c renders HTML and opens in browser
    (define-key map (kbd "C-c C-c") #'markdown-adaptive-preview-html)
    ;; Adaptive table management
    (define-key map (kbd "C-c C-t") #'markdown-adaptive-table-toggle)
    (define-key map (kbd "C-c C-x C-t") #'markdown-adaptive-table-mode)
    (define-key map (kbd "C-c C-x C-r") #'markdown-adaptive-table-render-all)
    map)
  "Keymap for `markdown-adaptive-ts-mode'.")

;;;###autoload
(define-derived-mode markdown-adaptive-ts-mode markdown-ts-mode "Markdown[TS+]"
  "Major mode extending `markdown-ts-mode' with adaptive tables and HTML preview.

Features:
1. Adaptive Tables:
   Tables are dynamically formatted and rendered with multi-line cell wrapping,
   strict column alignment, and clean box borders. Moving cursor into a table
   automatically reveals the raw Markdown for seamless editing.
2. HTML Preview (\\`C-c C-c'):
   Generates a polished, readable HTML preview written to /tmp and opens it
   directly in your web browser.

Keybindings:
\\{markdown-adaptive-ts-mode-map}"
  :group 'markdown-adaptive
  ;; Enable adaptive table mode by default
  (markdown-adaptive-table-mode 1))

;; Provide alias for convenience
;;;###autoload
(defalias 'custom-markdown-ts-mode #'markdown-adaptive-ts-mode)

(provide 'markdown-adaptive-ts-mode)

;;; markdown-adaptive-ts-mode.el ends here
