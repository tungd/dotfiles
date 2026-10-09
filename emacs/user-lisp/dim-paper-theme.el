;;; dim-paper-theme.el --- Dim paper with distinct syntax colors -*- lexical-binding: t; -*-

;;; Commentary:
;; A Modus derivative with the chosen dim-paper canvas and distinct syntax hues.
;; Keep typography separate: this theme does not change font size or spacing.

;;; Code:

(require 'modus-themes)
(require 'seq)

(defconst td/dim-paper-ansi-colors
  ["#111111" "#b02230" "#17643c" "#765800"
   "#1746b5" "#922b75" "#00666d" "#404040"
   "#353535" "#b42e3a" "#216e41" "#7c5b00"
   "#3057be" "#a13480" "#006c73" "#505050"]
  "Normal and bright ANSI colors shared with the Dim Paper Terminal profile.")

(defconst td/dim-paper-palette
  (let* ((overrides
          '((bg-main "#e3dfd5") (fg-main "#111111")
            (bg-dim "#d8d4ca") (fg-dim "#404040")
            (fg-alt "#1746b5") (bg-active "#c9c5bb")
            (bg-inactive "#d8d4ca") (border "#8a857b")
            (red "#b02230") (red-warmer "#93451a")
            (green "#17643c") (green-cooler "#216e41")
            (yellow "#765800") (yellow-warmer "#7c5b00")
            (blue "#1746b5") (blue-warmer "#3057be")
            (magenta "#922b75") (magenta-cooler "#6d32a8")
            (cyan "#00666d") (cyan-cooler "#006c73")
            (bg-hl-line "#dad6cc") (bg-region "#c9d3db")
            (bg-popup "#d8d4ca") (bg-completion "#c9d3db")
            (bg-mode-line-active "#c9c5bb")
            (bg-mode-line-inactive "#d8d4ca")
            (fg-mode-line-inactive fg-dim)
            (bg-tab-bar bg-dim) (bg-tab-current bg-main)
            (bg-tab-other bg-active) (bg-diff-context bg-dim)
            (cursor blue) (comment fg-dim) (docstring fg-dim)
            (builtin magenta-cooler) (fnname magenta) (fnname-call magenta)
            (constant magenta-cooler) (keyword blue) (type yellow)
            (string green) (variable red-warmer) (variable-use red-warmer)
            (fringe bg-main)
            (fg-line-number-inactive fg-dim)
            (fg-line-number-active blue)
            (bg-line-number-inactive bg-main)
            (bg-line-number-active bg-main)))
         (names '(black red green yellow blue magenta cyan white)))
    (dotimes (index 16)
      (let* ((name (nth (% index 8) names))
             (suffix (if (< index 8) "" "-bright"))
             (color (aref td/dim-paper-ansi-colors index)))
        (dolist (kind '(fg bg))
          (push (list (intern (format "%s-term-%s%s" kind name suffix)) color)
                overrides))))
    (append overrides
            (seq-remove (lambda (entry) (assq (car entry) overrides))
                        modus-themes-operandi-tinted-palette)))
  "Complete Modus palette with distinct syntax hues on dim paper.")

(defconst td/dim-paper-faces
  '(`(font-lock-keyword-face ((,c :foreground ,keyword :weight semibold)))
    `(font-lock-function-name-face ((,c :foreground ,fnname :weight semibold)))
    ;; Match normal ANSI black/white instead of Modus's bright substitutes.
    `(term-color-black ((,c :foreground ,fg-term-black :background ,bg-term-black)))
    `(term-color-white ((,c :foreground ,fg-term-white :background ,bg-term-white))))
  "Modus face templates for the Dim Paper derivative.")

;; Define each face once: duplicate settings can undo an override on enable.
(let* ((replaced-faces (mapcar #'caadr td/dim-paper-faces))
       (modus-themes-faces
        (seq-remove (lambda (entry) (memq (caadr entry) replaced-faces))
                    modus-themes-faces)))
  (modus-themes-theme
   'dim-paper 'td-themes
   "Dim paper background with distinct, readable syntax colors."
   'light 'td/dim-paper-palette nil nil 'td/dim-paper-faces))

(provide 'dim-paper-theme)
;;; dim-paper-theme.el ends here
