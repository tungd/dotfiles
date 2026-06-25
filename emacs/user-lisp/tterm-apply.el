;;; tterm-apply.el --- Apply plan buffer mutation for tterm -*- lexical-binding: t; -*-

;; Apply typed terminal plans to Emacs buffers.

(eval-and-compile
  (defconst tterm-apply--directory
    (file-name-directory (or load-file-name buffer-file-name default-directory))
    "Directory containing tterm apply Lisp files."))

(require 'cl-lib)
(require 'tterm-copy-mode (expand-file-name "tterm-copy-mode" tterm-apply--directory))
(require 'tterm-osc (expand-file-name "tterm-osc" tterm-apply--directory))
(require 'tterm-profiling (expand-file-name "tterm-profiling" tterm-apply--directory))

(defvar tterm--terminal)

(defvar-local tterm--cursor-row 0
  "Latest terminal cursor row.")

(defvar-local tterm--cursor-col 0
  "Latest terminal cursor column.")

(defvar-local tterm--cursor-visible t
  "Whether the latest terminal cursor is visible.")

(defvar-local tterm--cursor-shape 0
  "Latest terminal cursor shape.")

(defvar-local tterm--alt-screen nil
  "Whether the latest pulled terminal state is using the alternate screen.")

(defvar-local tterm--simple-row-map nil
  "Vector tracking rows where one buffer character equals one terminal cell.")

(defvar-local tterm--simple-row-prefix 0
  "Number of leading rows known to be fixed-width simple rows.")

(defun tterm--clamp-buffer-position (position)
  "Clamp POSITION to the current buffer."
  (min (max position (point-min)) (point-max)))

(defun tterm--copy-mode-point-state ()
  "Capture point state that terminal redraws must not steal in copy mode."
  (when tterm--copy-mode
    (list (cons (copy-marker (point)) (current-column))
          (mapcar
           (lambda (window)
             (save-excursion
               (goto-char (window-point window))
               (list window (copy-marker (point)) (current-column))))
           (get-buffer-window-list (current-buffer) nil t)))))

(defun tterm--restore-copy-mode-marker-column (marker column)
  "Move to MARKER's line at COLUMN, clamped to the current buffer."
  (goto-char (tterm--clamp-buffer-position (marker-position marker)))
  (move-to-column column))

(defun tterm--restore-copy-mode-point-state (state)
  "Restore copy-mode point STATE after terminal redraw mutation."
  (when state
    (let* ((point-state (car state))
           (point-marker (car point-state))
           (point-column (cdr point-state)))
      (unwind-protect
          (progn
            (tterm--restore-copy-mode-marker-column point-marker point-column)
            (dolist (entry (cadr state))
              (let ((window (nth 0 entry))
                    (position-marker (nth 1 entry))
                    (position-column (nth 2 entry)))
                (unwind-protect
                    (when (and (window-live-p window)
                               (eq (window-buffer window) (current-buffer)))
                      (save-excursion
                        (tterm--restore-copy-mode-marker-column
                         position-marker position-column)
                        (set-window-point window (point))))
                  (set-marker position-marker nil)))))
        (set-marker point-marker nil)))))

(declare-function tterm-alt-screen "tterm" (term))
(declare-function tterm-application-cursor "tterm" (term))
(declare-function tterm-cols "tterm" (term))
(declare-function tterm-set-alt-screen "tterm" (term value))
(declare-function tterm-set-application-cursor "tterm" (term value))
(declare-function tterm--active-hyperlink-p "tterm-osc" ())
(declare-function tterm--apply-active-hyperlink "tterm-osc" (start end))
(declare-function tterm--apply-background-face "tterm" (color))
(declare-function tterm--apply-color-name "tterm" (color &optional attribute))
(declare-function tterm--apply-foreground-face "tterm" (color))
(declare-function tterm--apply-hyperlink-span "tterm-osc" (row col len uri))
(declare-function tterm--apply-symbol-font-fallbacks-for-text "tterm" (text start end))
(declare-function tterm--copy-mode-promotable-scroll-p "tterm-copy-mode" (top bottom delta))
(declare-function tterm--copy-mode-promote-scrolled-lines "tterm-copy-mode" (row count))
(declare-function tterm--decode-apply-cell-text "tterm" (text cell-count))
(declare-function tterm--decode-apply-row-run-text "tterm" (text count cols))
(declare-function tterm--handle-osc-op "tterm-osc" (op))
(declare-function tterm--set-title "tterm-osc" (payload))

(defun tterm--apply-op-data (id ops)
  "Apply decoded apply OPS to the terminal buffer for terminal ID."
  (with-current-buffer (tterm--find-buffer id)
    (tterm--apply-profile-inc :calls)
    (tterm--apply-profile-inc :ops (length ops))
    (let ((copy-mode-point-state (tterm--copy-mode-point-state)))
      (unwind-protect
          (progn
            (save-excursion
              (save-restriction
                (let ((inhibit-redisplay t)
                      (inhibit-read-only t)
                      (inhibit-modification-hooks t)
                      (buffer-undo-list t))
                  (dolist (op ops)
                    (pcase (aref op 0)
                      ('A
                       (tterm--apply-profile-inc :plain-ops)
                       (tterm--apply-profile-time :plain-ms
                         (tterm--apply-plain-row-ops (aref op 1))))
                      ('B
                       (tterm--apply-profile-inc :styled-ops)
                       (tterm--apply-profile-time :styled-ms
                         (tterm--apply-styled-row-ops (aref op 1))))
                      ('R
                       (tterm--apply-profile-inc :full-row-run-ops)
                       (tterm--apply-full-row-run-op op))
                      ('C
                       (tterm--apply-profile-inc :cursor-ops)
                       (tterm--apply-profile-time :cursor-ms
                         (tterm--apply-cursor-op op)))
                      ('S
                       (tterm--apply-profile-inc :scroll-ops)
                       (tterm--apply-profile-time :scroll-ms
                         (tterm--apply-scroll-op op)))
                      ('Y
                       (tterm--apply-profile-inc :delete-row-ops)
                       (tterm--apply-profile-time :delete-row-ms
                         (tterm--delete-rows-op op)))
                      ('Z
                       (tterm--apply-profile-inc :insert-row-ops)
                       (tterm--apply-profile-time :insert-row-ms
                         (tterm--insert-rows-op op)))
                      ('T
                       (tterm--apply-profile-inc :title-ops)
                       (tterm--apply-profile-time :title-ms
                         (tterm--set-title (aref op 1))))
                      ('O
                       (tterm--apply-profile-inc :osc-ops)
                       (tterm--apply-profile-time :osc-ms
                         (tterm--handle-osc-op op)))
                      ('L
                       (tterm--apply-profile-inc :link-ops)
                       (tterm--apply-profile-time :link-ms
                         (tterm--apply-link-ops (aref op 1))))
                      ('U nil)
                      ('X (message "Terminal exited with status %d" (aref op 1)))
                      (_ nil))))))
            (tterm--sync-point-to-cursor))
        (tterm--restore-copy-mode-point-state copy-mode-point-state)))))

(defun tterm--initialize-screen (rows cols)
  "Initialize the current buffer to ROWS x COLS spaces."
  (let ((line (make-string cols ?\s))
        (inhibit-redisplay t)
        (inhibit-read-only t)
        (inhibit-modification-hooks t)
        (buffer-undo-list t))
    (erase-buffer)
    (dotimes (_ rows)
      (insert line "\n"))
    (setq-local tterm--simple-row-map (make-vector rows t))
    (setq-local tterm--simple-row-prefix rows)))

(defun tterm--find-buffer (id)
  "Find the buffer associated with terminal ID."
  (or (get-buffer (format "*tterm-%d*" id))
      (current-buffer)))

(defconst tterm--flat-style-run-field-count 13
  "Field count for one flat direct-bridge style run.")

(defun tterm--flat-style-color (runs base)
  "Return decoded color from flat RUNS at BASE."
  (pcase (aref runs base)
    (1 (cons 'palette (aref runs (1+ base))))
    (2 (list 'rgb
             (aref runs (1+ base))
             (aref runs (+ base 2))
             (aref runs (+ base 3))))
    (_ nil)))

(defun tterm--flat-style-face (runs base)
  "Return a face from flat direct-bridge RUNS at BASE."
  (let ((fg (tterm--flat-style-color runs (+ base 3)))
        (bg (tterm--flat-style-color runs (+ base 7)))
        face)
    (unless (zerop (aref runs base))
      (push '(:weight bold) face))
    (unless (zerop (aref runs (1+ base)))
      (push 'italic face))
    (unless (zerop (aref runs (+ base 2)))
      (push 'underline face))
    (when fg
      (push (tterm--apply-foreground-face fg) face))
    (when bg
      (push (tterm--apply-background-face bg) face))
    (nreverse (delq nil face))))

(defun tterm--nested-style-op-face (style)
  "Return a face from nested direct-bridge STYLE."
  (let (face)
    (when (aref style 0)
      (push '(:weight bold) face))
    (when (aref style 1)
      (push 'italic face))
    (when (aref style 2)
      (push 'underline face))
    (when (aref style 3)
      (push (tterm--apply-foreground-face (aref style 3)) face))
    (when (aref style 4)
      (push (tterm--apply-background-face (aref style 4)) face))
    (nreverse (delq nil face))))

(defun tterm--flat-styled-row-op-text (runs row-len)
  "Return propertized text from flat direct-bridge RUNS."
  (if (= (length runs) tterm--flat-style-run-field-count)
      (let* ((face (tterm--flat-style-face runs 0))
             (text (tterm--apply-profile-time :text-decode-ms
                     (tterm--decode-apply-cell-text
                      (aref runs 12) row-len))))
        (if face (propertize text 'face face) text))
    (let ((idx 0)
          chunks)
      (while (< idx (length runs))
        (let* ((face (tterm--flat-style-face runs idx))
               (text (tterm--apply-profile-time :text-decode-ms
                       (tterm--decode-apply-cell-text
                        (aref runs (+ idx 12)) (aref runs (+ idx 11))))))
          (push (if face (propertize text 'face face) text) chunks))
        (setq idx (+ idx tterm--flat-style-run-field-count)))
      (apply #'concat (nreverse chunks)))))

(defun tterm--nested-styled-row-op-text (runs row-len)
  "Return propertized text from nested direct-bridge RUNS."
  (if (= (length runs) 1)
      (let* ((run (aref runs 0))
             (face (tterm--nested-style-op-face (aref run 0)))
             (text (tterm--apply-profile-time :text-decode-ms
                     (tterm--decode-apply-cell-text (aref run 2) row-len))))
        (if face (propertize text 'face face) text))
    (let (chunks)
      (dotimes (idx (length runs))
        (let* ((run (aref runs idx))
               (face (tterm--nested-style-op-face (aref run 0)))
               (text (tterm--apply-profile-time :text-decode-ms
                       (tterm--decode-apply-cell-text
                        (aref run 2) (aref run 1)))))
          (push (if face (propertize text 'face face) text) chunks)))
      (apply #'concat (nreverse chunks)))))

(defun tterm--styled-row-op-text (runs row-len)
  "Return propertized text for direct-bridge RUNS."
  (if (or (= (length runs) 0)
          (integerp (aref runs 0)))
      (tterm--flat-styled-row-op-text runs row-len)
    (tterm--nested-styled-row-op-text runs row-len)))

(defun tterm--apply-styled-row-ops (rows)
  "Apply styled ROWS from an apply-ready B op."
  (let ((cols (and tterm--terminal (tterm-cols tterm--terminal)))
        current-row
        full-start-row
        full-next-row
        full-lines)
    (cl-labels
        ((flush-full-rows
          ()
          (when full-start-row
            (setq current-row
                  (tterm--replace-full-row-run
                   full-start-row
                   (nreverse full-lines)))
            (setq full-start-row nil
                  full-next-row nil
                  full-lines nil))))
      (dotimes (idx (length rows))
        (let* ((entry (aref rows idx))
               (row (tterm--shift-row (aref entry 0)))
               (col (aref entry 1))
               (row-len (aref entry 2))
               (text (tterm--styled-row-op-text (aref entry 3) row-len)))
          (tterm--apply-profile-inc :styled-rows)
          (if (and cols (= col 0) (= row-len cols)
                   (or (not full-start-row) (= row full-next-row)))
              (progn
                (unless full-start-row
                  (setq full-start-row row
                        full-next-row row))
                (push text full-lines)
                (setq full-next-row (1+ row)))
            (flush-full-rows)
            (if (and cols (= col 0) (= row-len cols))
                (setq full-start-row row
                      full-next-row (1+ row)
                      full-lines (list text))
              (setq current-row (tterm--goto-row row current-row))
              (tterm--replace-row-span-at-point row col row-len text cols)))))
      (flush-full-rows))))

(defun tterm--apply-full-row-run-op (op)
  "Apply decoded full-row run OP."
  (let* ((row (tterm--shift-row (aref op 1)))
         (count (aref op 2))
         (cols (and tterm--terminal (tterm-cols tterm--terminal)))
         (text (tterm--apply-profile-time :text-decode-ms
                 (tterm--decode-apply-row-run-text
                  (aref op 3) count cols))))
    (tterm--replace-full-row-run-text row count text)))

(defun tterm--apply-plain-row-ops (rows)
  "Apply ROWS from an apply-ready A op."
  (let ((cols (and tterm--terminal (tterm-cols tterm--terminal)))
        current-row
        full-start-row
        full-next-row
        full-lines)
    (cl-labels
        ((flush-full-rows
          ()
          (when full-start-row
            (setq current-row
                  (tterm--replace-full-row-run
                   full-start-row
                   (nreverse full-lines)))
            (setq full-start-row nil
                  full-next-row nil
                  full-lines nil))))
      (if (vectorp rows)
          (let ((idx 0)
                (count (length rows)))
            (while (< idx count)
              (let* ((entry (aref rows idx))
                     (row (aref entry 0))
                     (row (tterm--shift-row row))
                     (col (aref entry 1))
                     (row-len (aref entry 2))
                     (text (tterm--apply-profile-time :text-decode-ms
                             (tterm--decode-apply-cell-text
                              (aref entry 3) row-len))))
                (tterm--apply-profile-inc :plain-rows)
                (if (and cols (= col 0) (= row-len cols)
                         (or (not full-start-row) (= row full-next-row)))
                    (progn
                      (unless full-start-row
                        (setq full-start-row row
                              full-next-row row))
                      (push text full-lines)
                      (setq full-next-row (1+ row)))
                  (flush-full-rows)
                  (if (and cols (= col 0) (= row-len cols))
                      (setq full-start-row row
                            full-next-row (1+ row)
                            full-lines (list text))
                    (setq current-row (tterm--goto-row row current-row))
                    (tterm--replace-row-span-at-point row col row-len text cols))))
              (setq idx (1+ idx))))
        (dolist (entry rows)
          (let* ((row (aref entry 0))
                 (row (tterm--shift-row row))
                 (col (aref entry 1))
                 (row-len (aref entry 2))
                 (text (tterm--apply-profile-time :text-decode-ms
                         (tterm--decode-apply-cell-text
                          (aref entry 3) row-len))))
            (tterm--apply-profile-inc :plain-rows)
            (if (and cols (= col 0) (= row-len cols)
                     (or (not full-start-row) (= row full-next-row)))
                (progn
                  (unless full-start-row
                    (setq full-start-row row
                          full-next-row row))
                  (push text full-lines)
                  (setq full-next-row (1+ row)))
              (flush-full-rows)
              (if (and cols (= col 0) (= row-len cols))
                  (setq full-start-row row
                        full-next-row (1+ row)
                        full-lines (list text))
                (setq current-row (tterm--goto-row row current-row))
                (tterm--replace-row-span-at-point row col row-len text cols))))))
      (flush-full-rows))))

(defun tterm--apply-link-ops (links)
  "Apply decoded hyperlink LINKS from an apply-ready L op."
  (mapc
   (lambda (entry)
     (tterm--apply-profile-inc :link-spans)
     (tterm--apply-hyperlink-span
      (aref entry 0)
      (aref entry 1)
      (aref entry 2)
      (decode-coding-string (aref entry 3) 'utf-8 t)))
   links))

(defun tterm--goto-row (row current-row)
  "Move point to ROW, reusing CURRENT-ROW when rows are applied in order."
  (tterm--apply-profile-inc :row-navigation-calls)
  (tterm--apply-profile-time :row-navigation-ms
    (let ((delta (and current-row (- row current-row))))
      (cond
       ((and delta (>= delta 0) (<= delta 8))
        (forward-line delta))
       ((and (not current-row) (= row 0))
        (goto-char (point-min)))
       ((let ((position (tterm--simple-row-position row)))
          (when position
            (goto-char position)
            t)))
       ((and delta (>= delta 0))
        (forward-line delta))
       (t
        (goto-char (point-min))
        (forward-line row)))))
  row)

(defun tterm--read-u32 (payload offset)
  "Read a big-endian u32 from PAYLOAD at OFFSET."
  (logior (ash (aref payload offset) 24)
          (ash (aref payload (+ offset 1)) 16)
          (ash (aref payload (+ offset 2)) 8)
          (aref payload (+ offset 3))))

(defun tterm--read-i32 (payload offset)
  "Read a big-endian signed i32 from PAYLOAD at OFFSET."
  (let ((value (tterm--read-u32 payload offset)))
    (if (> value 2147483647)
        (- value 4294967296)
      value)))

(defun tterm--simple-span-text-p (text row-len)
  "Return non-nil when TEXT is one Emacs character per ROW-LEN cells."
  (and (= (length text) row-len)
       (not (multibyte-string-p text))))

(defun tterm--simple-row-p (row)
  "Return non-nil when ROW can use direct character offsets."
  (and (vectorp tterm--simple-row-map)
       (>= row 0)
       (< row (length tterm--simple-row-map))
       (aref tterm--simple-row-map row)))

(defun tterm--simple-row-position (row)
  "Return direct buffer position for ROW when its prefix is fixed-width."
  (let ((cols (and tterm--terminal (tterm-cols tterm--terminal))))
    (and (vectorp tterm--simple-row-map)
         cols
         (>= row 0)
         (<= row tterm--simple-row-prefix)
         (<= row (length tterm--simple-row-map))
         (+ (point-min) (* row (1+ cols))))))

(defun tterm--recompute-simple-row-prefix ()
  "Recompute the leading fixed-width row count."
  (setq-local tterm--simple-row-prefix 0)
  (when (vectorp tterm--simple-row-map)
    (let ((row 0)
          (rows (length tterm--simple-row-map)))
      (while (and (< row rows)
                  (aref tterm--simple-row-map row))
        (setq row (1+ row)))
      (setq-local tterm--simple-row-prefix row))))

(defun tterm--clear-simple-row-map ()
  "Clear fixed-width row metadata."
  (setq-local tterm--simple-row-map nil)
  (setq-local tterm--simple-row-prefix 0))

(defun tterm--set-simple-row (row simple)
  "Record whether ROW can use direct character offsets."
  (when (and (vectorp tterm--simple-row-map)
             (>= row 0)
             (< row (length tterm--simple-row-map)))
    (let ((old (aref tterm--simple-row-map row)))
      (unless (eq old simple)
        (aset tterm--simple-row-map row simple)
        (cond
         ((not simple)
          (when (< row tterm--simple-row-prefix)
            (setq-local tterm--simple-row-prefix row)))
         ((= row tterm--simple-row-prefix)
          (let ((next row)
                (rows (length tterm--simple-row-map)))
            (while (and (< next rows)
                        (aref tterm--simple-row-map next))
              (setq next (1+ next)))
            (setq-local tterm--simple-row-prefix next))))))))

(defun tterm--set-simple-row-range (start-row lines)
  "Update simple-row metadata for LINES beginning at START-ROW."
  (let ((cols (and tterm--terminal (tterm-cols tterm--terminal)))
        (row start-row))
    (dolist (line lines)
      (tterm--set-simple-row
       row
       (and cols (tterm--simple-span-text-p line cols)))
      (setq row (1+ row)))))

(defun tterm--scroll-simple-row-map (top bottom delta count)
  "Update simple-row metadata for a scroll from TOP to BOTTOM by DELTA."
  (when (and (vectorp tterm--simple-row-map)
             (> count 0)
             (>= top 0)
             (<= bottom (length tterm--simple-row-map)))
    (cond
     ((< delta 0)
      (let ((row top))
        (while (< row (- bottom count))
          (aset tterm--simple-row-map row
                (aref tterm--simple-row-map (+ row count)))
          (setq row (1+ row))))
      (let ((row (- bottom count)))
        (while (< row bottom)
          (aset tterm--simple-row-map row t)
          (setq row (1+ row)))))
     ((> delta 0)
      (let ((row (1- bottom)))
        (while (>= row (+ top count))
          (aset tterm--simple-row-map row
                (aref tterm--simple-row-map (- row count)))
          (setq row (1- row))))
      (let ((row top))
        (while (< row (+ top count))
          (aset tterm--simple-row-map row t)
          (setq row (1+ row))))))
    (tterm--recompute-simple-row-prefix)))

(defun tterm--replace-row-span (row col row-len text)
  "Replace ROW span at COL covering ROW-LEN cells with TEXT."
  (tterm--goto-row row nil)
  (tterm--replace-row-span-at-point row col row-len text))

(defun tterm--replace-full-row-run (start-row lines)
  "Replace consecutive full rows from START-ROW with LINES.
Return the row immediately after the replaced run, with point there."
  (tterm--replace-full-row-run-text
   start-row
   (length lines)
   (if lines (mapconcat #'identity lines "\n") "")))

(defun tterm--replace-full-row-run-text (start-row count text)
  "Replace COUNT consecutive full rows from START-ROW with TEXT.
TEXT contains rows separated by newlines, without a trailing newline.
Return the row immediately after the replaced run, with point there."
  (tterm--apply-profile-inc :full-row-runs)
  (tterm--apply-profile-inc :full-row-lines count)
  (tterm--apply-profile-time :full-row-ms
    (let ((position (and (/= start-row 0)
                         (tterm--simple-row-position start-row))))
      (cond
       ((= start-row 0)
        (goto-char (point-min)))
       (position
        (goto-char position))
       (t
        (goto-char (point-min))
        (forward-line start-row))))
    (let ((start (point)))
      (let ((end-position (tterm--simple-row-position (+ start-row count))))
        (if end-position
            (goto-char end-position)
          (forward-line count)))
      (delete-region start (point))
      (goto-char start)
      (insert (if (> count 0) (concat text "\n") ""))
      (tterm--set-simple-row-range start-row (split-string text "\n"))
      (let ((end (point)))
        (tterm--apply-symbol-font-fallbacks-for-text text start end)
        (when (and (tterm--active-hyperlink-p)
                   (> end start))
          (tterm--apply-profile-time :link-property-ms
            (tterm--apply-active-hyperlink start (max start (1- end))))))
      (+ start-row count))))

(defun tterm--replace-row-span-at-point (row col row-len text &optional cols)
  "Replace ROW span at COL covering ROW-LEN cells with TEXT.
Point must be at the beginning of ROW."
  (tterm--apply-profile-inc :partial-spans)
  (tterm--apply-profile-time :partial-span-ms
    (let* ((row-was-simple (tterm--simple-row-p row))
           (text-simple (tterm--simple-span-text-p text row-len))
           (cols (or cols (and tterm--terminal (tterm-cols tterm--terminal))))
           start)
      (if (and row-was-simple text-simple)
          (progn
            (setq start (+ (point) col))
            (goto-char (+ start row-len))
            (delete-region start (point))
            (goto-char start))
        (unless (= col 0)
          (move-to-column col))
        (setq start (point))
        (move-to-column (+ col row-len))
        (delete-region start (point))
        (goto-char start))
      (insert text)
      (tterm--set-simple-row
       row
       (cond
        ((and cols (= col 0) (= row-len cols)) text-simple)
        (row-was-simple text-simple)
        (t nil)))
      (let ((end (point)))
        (tterm--apply-symbol-font-fallbacks-for-text text start end)
        (when (tterm--active-hyperlink-p)
          (tterm--apply-profile-time :link-property-ms
            (tterm--apply-active-hyperlink start end)))))))

(defun tterm--apply-cursor (payload)
  "Apply cursor frame PAYLOAD."
  (tterm--apply-cursor-range payload 0 (length payload)))

(defun tterm--apply-cursor-range (payload payload-start payload-end)
  "Apply cursor frame in PAYLOAD between PAYLOAD-START and PAYLOAD-END."
  (when (>= (- payload-end payload-start) 10)
    (let* ((row (tterm--read-u32 payload payload-start))
           (row (tterm--shift-row row))
           (col (tterm--read-u32 payload (+ payload-start 4)))
           (visible (aref payload (+ payload-start 8)))
           (shape (aref payload (+ payload-start 9)))
           (application-cursor
            (and (> (- payload-end payload-start) 10)
                 (/= (aref payload (+ payload-start 10)) 0)))
           (alt-screen
            (and (> (- payload-end payload-start) 11)
                 (/= (aref payload (+ payload-start 11)) 0))))
      (tterm--apply-cursor-state
       row col (/= visible 0) shape application-cursor alt-screen))))

(defun tterm--apply-cursor-op (op)
  "Apply cursor OP."
  (tterm--apply-cursor-state (aref op 1)
                             (aref op 2) (aref op 3) (aref op 4)
                             (and (> (length op) 5) (aref op 5))
                             (and (> (length op) 6) (aref op 6))))

(defun tterm--apply-cursor-state (row col visible shape application-cursor alt-screen)
  "Apply cursor state ROW COL VISIBLE SHAPE APPLICATION-CURSOR and ALT-SCREEN."
  (setq-local tterm--cursor-row row
              tterm--cursor-col col
              tterm--cursor-visible visible
              tterm--cursor-shape shape
              tterm--alt-screen alt-screen)
  (when tterm--terminal
    (tterm-set-application-cursor tterm--terminal application-cursor)
    (tterm-set-alt-screen tterm--terminal alt-screen))
  (setq-local cursor-type 'box)
  (tterm--sync-point-to-cursor))

(defun tterm--sync-point-to-cursor ()
  "Move point to the terminal cursor unless copy mode owns navigation."
  (tterm--apply-profile-time :cursor-sync-ms
    (when (and (eq major-mode 'tterm-mode)
               (not tterm--copy-mode))
      (let ((target
             (save-excursion
               (let ((position (tterm--simple-row-position tterm--cursor-row)))
                 (if position
                     (progn
                       (goto-char position)
                       (if (tterm--simple-row-p tterm--cursor-row)
                           (goto-char
                            (+ position
                               (min tterm--cursor-col (tterm-cols tterm--terminal))))
                         (move-to-column tterm--cursor-col)))
                   (goto-char (point-min))
                   (forward-line tterm--cursor-row)
                   (move-to-column tterm--cursor-col)))
               (point))))
        (goto-char target)
        (dolist (window (get-buffer-window-list (current-buffer) nil t))
          (when (window-live-p window)
            (set-window-point window target)))))))

(defun tterm--blank-line ()
  "Return a blank terminal row for the current buffer."
  (make-string (max 0 (or (and tterm--terminal (tterm-cols tterm--terminal))
                          (window-body-width)))
               ?\s))

(defun tterm--blank-lines (count)
  "Return COUNT blank terminal rows, including trailing newlines."
  (let* ((blank (tterm--blank-line))
         (line-len (length blank))
         (stride (1+ line-len))
         (text (make-string (* count stride) ?\s)))
    (dotimes (idx count)
      (aset text (+ (* idx stride) line-len) ?\n))
    text))

(defun tterm--apply-scroll (payload)
  "Apply scroll frame PAYLOAD."
  (tterm--apply-scroll-range payload 0 (length payload)))

(defun tterm--delete-lines-at (row count)
  "Delete COUNT full lines starting at ROW."
  (goto-char (point-min))
  (forward-line row)
  (let ((start (point)))
    (forward-line count)
    (delete-region start (point))))

(defun tterm--apply-scroll-range (payload payload-start payload-end)
  "Apply scroll frame in PAYLOAD between PAYLOAD-START and PAYLOAD-END."
  (when (>= (- payload-end payload-start) 12)
    (tterm--apply-scroll-decoded
     (tterm--read-u32 payload payload-start)
     (tterm--read-u32 payload (+ payload-start 4))
     (tterm--read-i32 payload (+ payload-start 8)))))

(defun tterm--apply-scroll-op (op)
  "Apply scroll OP."
  (tterm--apply-scroll-decoded (aref op 1) (aref op 2) (aref op 3)))

(defun tterm--apply-scroll-decoded (top bottom delta)
  "Apply decoded terminal scroll TOP BOTTOM DELTA."
  (when (tterm--copy-mode-promotable-scroll-p top bottom delta)
    (tterm--copy-mode-promote-scrolled-lines
     (tterm--shift-row top)
     (min (- bottom top) (- delta))))
  (tterm--apply-scroll-values (tterm--shift-row top)
                              (tterm--shift-row bottom)
                              delta))

(defun tterm--apply-scroll-values (top bottom delta)
  "Apply scroll region TOP BOTTOM DELTA."
  (let* ((height (- bottom top))
         (count (min height (abs delta)))
         (blank-lines (tterm--blank-lines count))
         (inhibit-read-only t))
    (when (> count 0)
      (save-excursion
        (cond
         ((< delta 0)
          (tterm--delete-lines-at top count)
          (goto-char (point-min))
          (forward-line (- bottom count))
          (insert blank-lines))
         ((> delta 0)
          (goto-char (point-min))
          (forward-line top)
          (insert blank-lines)
          (tterm--delete-lines-at bottom count))))
      (tterm--scroll-simple-row-map top bottom delta count))))

(defun tterm--delete-rows-op (op)
  "Apply delete rows OP."
  (tterm--delete-lines-at (tterm--shift-row (aref op 1)) (aref op 2))
  (tterm--clear-simple-row-map))

(defun tterm--insert-rows-op (op)
  "Apply insert rows OP."
  (goto-char (point-min))
  (forward-line (tterm--shift-row (aref op 1)))
  (dotimes (_ (aref op 2))
    (insert "\n"))
  (tterm--clear-simple-row-map))

(defun tterm--delete-rows (payload)
  "Delete rows frame PAYLOAD."
  (when (>= (length payload) 8)
    (let* ((start-row (logior (ash (aref payload 0) 24) (ash (aref payload 1) 16)
                              (ash (aref payload 2) 8) (aref payload 3)))
           (start-row (tterm--shift-row start-row))
           (count (logior (ash (aref payload 4) 24) (ash (aref payload 5) 16)
                          (ash (aref payload 6) 8) (aref payload 7))))
      (save-excursion
        (goto-char (point-min))
        (forward-line start-row)
        (let ((start (point)))
          (forward-line count)
          (delete-region start (point))))
      (tterm--clear-simple-row-map))))

(defun tterm--insert-rows (payload)
  "Insert rows frame PAYLOAD."
  (when (>= (length payload) 8)
    (let* ((start-row (logior (ash (aref payload 0) 24) (ash (aref payload 1) 16)
                              (ash (aref payload 2) 8) (aref payload 3)))
           (start-row (tterm--shift-row start-row))
           (count (logior (ash (aref payload 4) 24) (ash (aref payload 5) 16)
                          (ash (aref payload 6) 8) (aref payload 7))))
      (save-excursion
        (goto-char (point-min))
        (forward-line start-row)
        (dotimes (_ count)
          (insert "\n")))
      (tterm--clear-simple-row-map))))

(provide 'tterm-apply)
;;; tterm-apply.el ends here
