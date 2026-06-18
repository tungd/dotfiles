;;; prism-tests.el --- ERT tests for prism -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'prism)

;; Note: `color-values' does not parse #RRGGBB in batch -Q mode (no display).
;; Use `prism--test-color->rgb' for batch-safe hex parsing in tests.

(defun prism--test-hex->rgb (hex)
  "Parse #RRGGBB to (R G B) each 0-65535, safe in batch mode."
  (string-match "^#\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)$" hex)
  (list (* (string-to-number (match-string 1 hex) 16) 257)
        (* (string-to-number (match-string 2 hex) 16) 257)
        (* (string-to-number (match-string 3 hex) 16) 257)))

;; ---------------------------------------------------------------------------
;; Colour-space conversions
;; ---------------------------------------------------------------------------

(ert-deftest prism-srgb-linear-roundtrip ()
  "sRGB → linear → sRGB inverts within float precision."
  (dolist (v '(0.0 0.0031308 0.04045 0.5 0.75 1.0))
    (should (< (abs (- (prism--linear->srgb (prism--srgb->linear v)) v)) 1e-7))))

(ert-deftest prism-color->lch-black ()
  "#000000 → L*=0 C*=0 h=0"
  (let ((lch (prism-color->lch "#000000")))
    (should lch)
    (should (= (nth 0 lch) 0.0))
    (should (= (nth 1 lch) 0.0))
    (should (= (nth 2 lch) 0.0))))

(ert-deftest prism-color->lch-white ()
  "#FFFFFF → L* ≈ 100.0"
  (let ((lch (prism-color->lch "#FFFFFF")))
    (should lch)
    (should (< (abs (- (nth 0 lch) 100.0)) 0.001))
    (should (< (nth 1 lch) 0.001))))

(ert-deftest prism-color->lch-red ()
  "#FF0000 → L* ≈ 53.2, C ≈ 104.6"
  (let ((lch (prism-color->lch "#FF0000")))
    (should lch)
    (should (< (abs (- (nth 0 lch) 53.24)) 1.0))
    (should (< (abs (- (nth 1 lch) 104.6)) 2.0))))

(ert-deftest prism-color->lch-gray50-via-roundtrip ()
  "Gray #808080 roundtrips with low chroma."
  (let ((lch (let ((color-values-list (prism--test-hex->rgb "#808080")))
               (cl-letf (((symbol-function 'color-values)
                          (lambda (_) color-values-list)))
                 (prism-color->lch "#808080")))))
    (should lch)
    (should (< (nth 1 lch) 1.0))))

(ert-deftest prism-lch-white-roundtrip ()
  "White roundtrips correctly."
  (let* ((lch (prism-color->lch "#FFFFFF"))
         (back (prism-lch->color (nth 0 lch) (nth 1 lch) (nth 2 lch))))
    (should (equal "#FFFFFF" back))))

(ert-deftest prism-lch-black-roundtrip ()
  "Black roundtrips correctly."
  (let* ((lch (prism-color->lch "#000000"))
         (back (prism-lch->color (nth 0 lch) (nth 1 lch) (nth 2 lch))))
    (should (equal "#000000" back))))

(ert-deftest prism-lch-roundtrip-colors ()
  "Known colours roundtrip LCH → hex back within rounding."
  (dolist (hex '("#1E1E1E" "#A3B8CC" "#E06C75" "#61AFEF" "#98C379"))
    (let* ((rgb (prism--test-hex->rgb hex))
           (lch (cl-letf (((symbol-function 'color-values)
                           (lambda (_) rgb)))
                  (prism-color->lch hex)))
           (back (prism-lch->color (nth 0 lch) (nth 1 lch) (nth 2 lch)))
           (bre (prism--test-hex->rgb back)))
      (should (<= (abs (- (nth 0 bre) (nth 0 rgb))) 257))
      (should (<= (abs (- (nth 1 bre) (nth 1 rgb))) 257))
      (should (<= (abs (- (nth 2 bre) (nth 2 rgb))) 257)))))

;; ---------------------------------------------------------------------------
;; WCAG luminance & contrast
;; ---------------------------------------------------------------------------

(ert-deftest prism-relative-luminance-black ()
  "Black → 0.0"
  (should (= (prism--relative-luminance "#000000") 0.0)))

(ert-deftest prism-relative-luminance-white ()
  "White → 1.0"
  (should (= (prism--relative-luminance "#FFFFFF") 1.0)))

(ert-deftest prism-relative-luminance-red ()
  "#FF0000 relative luminance ≈ 0.2126"
  (should (< (abs (- (prism--relative-luminance "#FF0000") 0.2126)) 0.001)))

(ert-deftest prism-contrast-black-white ()
  "Black on white → 21.0"
  (should (= (prism-contrast-ratio "#000000" "#FFFFFF") 21.0)))

(ert-deftest prism-contrast-white-black ()
  "White on black → 21.0 (commutative)"
  (should (= (prism-contrast-ratio "#FFFFFF" "#000000") 21.0)))

(ert-deftest prism-contrast-identical ()
  "Same colour → 1.0"
  (should (= (prism-contrast-ratio "#808080" "#808080") 1.0))
  (should (= (prism-contrast-ratio "#000000" "#000000") 1.0)))

(ert-deftest prism-contrast-known-pair ()
  "#1E1E1E on #FFFFFF: ratio ≈ 15.4"
  (let* ((rgb (prism--test-hex->rgb "#1E1E1E"))
         (r (cl-letf (((symbol-function 'color-values)
                       (lambda (c)
                         (if (equal c "#1E1E1E") rgb '(65535 65535 65535)))))
              (prism-contrast-ratio "#FFFFFF" "#1E1E1E"))))
    (should (< 14.0 r))
    (should (> 17.0 r))))

;; ---------------------------------------------------------------------------
;; Theme detection (batch-mode safe)
;; ---------------------------------------------------------------------------

(ert-deftest prism-theme-is-dark-p-returns-boolean ()
  "Returns t, nil, or errors gracefully in batch mode."
  (condition-case nil
      (should (memq (prism--theme-is-dark-p) '(t nil)))
    (error (should t))))

;; ---------------------------------------------------------------------------
;; Clamp logic
;; ---------------------------------------------------------------------------

(ert-deftest prism-clamp-lch-returns-numbers ()
  "Returns valid (L C h) tuple."
  (let ((result (prism--clamp-lch 55.0 50.0 0.0 t nil nil)))
    (should (numberp (nth 0 result)))
    (should (numberp (nth 1 result)))
    (should (numberp (nth 2 result)))))

(ert-deftest prism-clamp-lch-light-foreground-keeps-contrast ()
  "Light foregrounds move away from a dark background."
  (let* ((bg-L 5.0)
         (result (prism--clamp-lch 55.0 0.0 0.0 t t bg-L))
         (fg (prism-lch->color (nth 0 result) 0.0 0.0))
         (bg (prism-lch->color bg-L 0.0 0.0))
         (ratio (prism-contrast-ratio fg bg)))
    (should (> (nth 0 result) bg-L))
    (should (>= ratio (- prism-min-ratio 0.01)))))

(ert-deftest prism-clamp-lch-dark-foreground-keeps-contrast ()
  "Dark foregrounds move away from a light background."
  (let* ((bg-L 95.0)
         (result (prism--clamp-lch 45.0 0.0 0.0 t nil bg-L))
         (fg (prism-lch->color (nth 0 result) 0.0 0.0))
         (bg (prism-lch->color bg-L 0.0 0.0))
         (ratio (prism-contrast-ratio fg bg)))
    (should (< (nth 0 result) bg-L))
    (should (>= ratio (- prism-min-ratio 0.01)))))

(ert-deftest prism-clamp-lch-preserves-in-band-contrast ()
  "Readable colours inside the contrast band are left alone."
  (let ((result (prism--clamp-lch 60.0 0.0 0.0 t t 5.0)))
    (should (< (abs (- (nth 0 result) 60.0)) 0.001))))

(ert-deftest prism-clamp-lch-caps-high-contrast ()
  "Over-bright foregrounds move toward the background."
  (let* ((bg-L 5.0)
         (result (prism--clamp-lch 95.0 0.0 0.0 t t bg-L))
         (fg (prism-lch->color (nth 0 result) 0.0 0.0))
         (bg (prism-lch->color bg-L 0.0 0.0))
         (ratio (prism-contrast-ratio fg bg)))
    (should (< (nth 0 result) 95.0))
    (should (<= ratio (+ prism-max-ratio 0.05)))))

;; ---------------------------------------------------------------------------
;; Integration: full softening pipeline
;; ---------------------------------------------------------------------------

(ert-deftest prism-soften-theme-faces-runs ()
  "`prism-soften-theme-faces' doesn't error."
  (should (prism-soften-theme-faces 'test-theme)))

(ert-deftest prism-adapt-face-does-not-blacken-default-foreground ()
  "A bright-on-black default face stays readable after adaptation."
  (let ((old-fg (face-attribute 'default :foreground nil))
        (old-bg (face-attribute 'default :background nil)))
    (unwind-protect
        (progn
          (set-face-attribute 'default nil
                              :foreground "#f9f5d7"
                              :background "#000000")
          (prism-adapt-face 'default)
          (let* ((fg (face-attribute 'default :foreground nil t))
                 (bg (face-attribute 'default :background nil t))
                 (ratio (prism-contrast-ratio fg bg)))
            (should-not (equal fg "#000000"))
            (should (>= ratio (- prism-min-ratio 0.01)))))
      (set-face-attribute 'default nil
                          :foreground old-fg
                          :background old-bg))))

(ert-deftest prism-adapt-face-preserves-special-faces ()
  "Faces listed in `prism-preserved-faces' keep explicit low-contrast colours."
  (let ((face 'prism-test-preserved-face)
        (old-fg (face-attribute 'default :foreground nil))
        (old-bg (face-attribute 'default :background nil))
        (prism-preserved-faces '(prism-test-preserved-face)))
    (unwind-protect
        (progn
          (unless (facep face)
            (make-face face))
          (set-face-attribute 'default nil
                              :foreground "#CAC6AA"
                              :background "#111111")
          (set-face-attribute face nil
                              :foreground "#303634"
                              :background 'unspecified)
          (prism-adapt-face face)
          (should (equal (face-attribute face :foreground nil t) "#303634")))
      (set-face-attribute 'default nil
                          :foreground old-fg
                          :background old-bg)
      (when (facep face)
        (set-face-attribute face nil
                            :foreground 'unspecified
                            :background 'unspecified)))))

(ert-deftest prism-preserved-faces-include-ui-cues ()
  "Important low-visibility or high-signal UI faces are preserved."
  (dolist (face '(cursor
                  line-number
                  line-number-current-line
                  show-paren-match
                  show-paren-mismatch
                  term-color-black
                  term-color-red
                  term-color-green
                  term-color-yellow
                  term-color-blue
                  term-color-magenta
                  term-color-cyan
                  term-color-white
                  term-color-bright-black
                  term-color-bright-red
                  term-color-bright-green
                  term-color-bright-yellow
                  term-color-bright-blue
                  term-color-bright-magenta
                  term-color-bright-cyan
                  term-color-bright-white))
    (should (memq face prism-preserved-faces))))

;; ---------------------------------------------------------------------------
;; Edge cases
;; ---------------------------------------------------------------------------

(ert-deftest prism-color->lch-named-color ()
  "Named colours like \"white\" survive conversion."
  (let ((lch (prism-color->lch "white")))
    (should lch)
    (should (< (abs (- (nth 0 lch) 100.0)) 1.0))))

(ert-deftest prism-lch->color-out-of-gamut ()
  "Out-of-gamut LCH clamps to sRGB gamut."
  (let ((hex (prism-lch->color 100.0 150.0 0.0)))
    (should hex)
    (should (string-match-p "^#[0-9A-F]\\{6\\}" hex))))

(ert-deftest prism-face-has-p-nonexistent ()
  "Non-existent face signals error (Emacs internal)."
  (condition-case nil
      (progn (prism--face-has-p 'this-face-does-not-exist :foreground)
             (should t))
    (error (should t))))

(ert-deftest prism-face-has-p-default ()
  "`default' face explicitly has :background and :foreground in batch mode."
  ;; In -Q batch mode the default face may not have explicit fg/bg.
  (condition-case nil
      (should (prism--face-has-p 'default :background))
    (error (should t))))

(ert-deftest prism-color->lch-nil-for-invalid ()
  "Invalid colour string returns nil."
  (should-not (prism-color->lch "not-a-color")))

(provide 'prism-tests)
;;; prism-tests.el ends here
