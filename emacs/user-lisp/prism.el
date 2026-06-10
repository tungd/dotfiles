;;; prism.el --- Adapt theme faces for reduced eye strain -*- lexical-binding: t; -*-
;;
;; After-load-theme hook that walks all themed faces, clamps perceptual
;; lightness (L*) to a safe range, and enforces a minimum/maximum WCAG
;; contrast ratio.  Works in CIE L*C*h° to preserve the theme author's
;; hue/chroma choices.
;;
;; Install:
;;   (add-hook 'enable-theme-functions #'prism-soften-theme-faces)
;;
;; Customize:
;;   prism-dark-min-bg-L  prism-dark-max-bg-L
;;   prism-dark-min-fg-L  prism-dark-max-fg-L
;;   prism-light-*  (mirror)
;;   prism-min-ratio        prism-max-ratio

;;; Code:

(require 'cl-lib)

;; ---------------------------------------------------------------------------
;; Configurable parameters
;; ---------------------------------------------------------------------------

(defgroup prism nil
  "Theme face softening."
  :group 'faces)

(defcustom prism-dark-min-bg-L 5
  "Darkest allowed background L* (dark themes)."
  :type 'float :group 'prism)
(defcustom prism-dark-max-bg-L 22
  "Lightest allowed background L* (dark themes)."
  :type 'float :group 'prism)
(defcustom prism-dark-min-fg-L 50
  "Lowest allowed foreground L* (dark themes)."
  :type 'float :group 'prism)
(defcustom prism-dark-max-fg-L 88
  "Highest allowed foreground L* (dark themes)."
  :type 'float :group 'prism)

(defcustom prism-light-min-bg-L 80
  "Darkest allowed background L* (light themes)."
  :type 'float :group 'prism)
(defcustom prism-light-max-bg-L 96
  "Lightest allowed background L* (light themes)."
  :type 'float :group 'prism)
(defcustom prism-light-min-fg-L 15
  "Lowest allowed foreground L* (light themes)."
  :type 'float :group 'prism)
(defcustom prism-light-max-fg-L 55
  "Highest allowed foreground L* (light themes)."
  :type 'float :group 'prism)

(defcustom prism-min-ratio 5.5
  "Minimum WCAG contrast ratio."
  :type 'float :group 'prism)
(defcustom prism-max-ratio 11.0
  "Maximum WCAG contrast ratio; higher values risk halation."
  :type 'float :group 'prism)

(defcustom prism-preserved-faces
  '(cursor
    fringe
    line-number
    line-number-current-line
    show-paren-match
    show-paren-mismatch)
  "Faces whose explicit colours should not be changed by prism."
  :type '(repeat symbol)
  :group 'prism)

;; ---------------------------------------------------------------------------
;; Colour-space conversion  (sRGB D65 → linear → XYZ → L*a*b* → L*C*h°)
;; ---------------------------------------------------------------------------

(defun prism--srgb->linear (c)
  "One sRGB channel [0,1] to linear."
  (if (<= c 0.04045) (/ c 12.92) (expt (/ (+ c 0.055) 1.055) 2.4)))

(defun prism--linear->srgb (c)
  "One linear channel [0,1] to sRGB."
  (if (<= c 0.0031308) (* c 12.92) (- (* 1.055 (expt c (/ 1.0 2.4))) 0.055)))

(defun prism--hex-color-values (color)
  "Return 16-bit RGB values for hex COLOR, or nil."
  (when (and (stringp color)
             (string-match "\\`#\\([[:xdigit:]]+\\)\\'" color))
    (let* ((hex (match-string 1 color))
           (len (length hex)))
      (when (and (= (% len 3) 0)
                 (<= 3 len 12))
        (let* ((digits (/ len 3))
               (max-channel (1- (expt 16 digits)))
               (scale (/ 65535.0 max-channel)))
          (cl-loop for i below 3
                   for start = (* i digits)
                   collect (round
                            (* (string-to-number
                                (substring hex start (+ start digits))
                                16)
                               scale))))))))

(defun prism--color-values (color)
  "Return 16-bit RGB values for COLOR."
  (or (prism--hex-color-values color)
      (color-values color)))

(defun prism-color->lch (color)
  "Return (L* C* h°) for COLOR (string or Emacs color spec), or nil."
  (let ((rgb (prism--color-values color)))        ; (R G B) each 0-65535
    (when rgb
      (let* ((r (prism--srgb->linear (/ (float (nth 0 rgb)) 65535)))
             (g (prism--srgb->linear (/ (float (nth 1 rgb)) 65535)))
             (b (prism--srgb->linear (/ (float (nth 2 rgb)) 65535)))
             ;; sRGB D65 → XYZ
             (x (+ (* 0.4124564 r) (* 0.3575761 g) (* 0.1804375 b)))
             (y (+ (* 0.2126729 r) (* 0.7151522 g) (* 0.0721750 b)))
             (z (+ (* 0.0193339 r) (* 0.1191920 g) (* 0.9503041 b)))
             ;; D65 reference white
             (xn 0.95047) (yn 1.00000) (zn 1.08883)
             (fx (if (> x 0.008856) (expt (/ x xn) (/ 1.0 3.0))
                   (+ (* (/ 7.787 xn) x) (/ 16.0 116.0))))
             (fy (if (> y 0.008856) (expt (/ y yn) (/ 1.0 3.0))
                   (+ (* (/ 7.787 yn) y) (/ 16.0 116.0))))
             (fz (if (> z 0.008856) (expt (/ z zn) (/ 1.0 3.0))
                   (+ (* (/ 7.787 zn) z) (/ 16.0 116.0))))
             (L (- (* 116 fy) 16))
             (a (* 500 (- fx fy)))
             (b* (* 200 (- fy fz)))
             (C (sqrt (+ (* a a) (* b* b*))))
             (h (if (and (= a 0) (= b* 0)) 0
                  (let ((deg (atan b* a)))
                    (if (< deg 0) (+ deg (* 2 float-pi)) deg))))
             (h-deg (* h (/ 180 float-pi))))
        (list L C h-deg)))))

(defun prism-lch->color (L C h-deg)
  "Return \"#RRGGBB\" string from L* C* h° (h in degrees), or nil."
  (let* ((h-rad (* h-deg (/ float-pi 180)))
         (a (* C (cos h-rad)))
         (b* (* C (sin h-rad)))
         (xn 0.95047) (yn 1.00000) (zn 1.08883)
         (fy (/ (+ L 16) 116.0))
         (fx (+ (/ a 500.0) fy))
         (fz (- fy (/ b* 200.0)))
         (x (if (> (expt fx 3) 0.008856) (* xn (expt fx 3))
              (* xn (* (- fx (/ 4.0 29.0)) (/ 108.0 841.0)))))
         (y (if (> L 7.999627) (* yn (expt fy 3))
              (* yn (* (- fy (/ 4.0 29.0)) (/ 108.0 841.0)))))
         (z (if (> (expt fz 3) 0.008856) (* zn (expt fz 3))
              (* zn (* (- fz (/ 4.0 29.0)) (/ 108.0 841.0)))))
         (r (prism--linear->srgb (max 0 (min 1 (+ (*  3.2404542 x) (* -1.5371385 y) (* -0.4985314 z))))))
         (g (prism--linear->srgb (max 0 (min 1 (+ (* -0.9692660 x) (*  1.8760108 y) (*  0.0415560 z))))))
         (b (prism--linear->srgb (max 0 (min 1 (+ (*  0.0556434 x) (* -0.2040259 y) (*  1.0572252 z))))))
         (ri (max 0 (min 255 (round (* r 255)))))
         (gi (max 0 (min 255 (round (* g 255)))))
         (bi (max 0 (min 255 (round (* b 255))))))
    (format "#%02X%02X%02X" ri gi bi)))

;; ---------------------------------------------------------------------------
;; WCAG contrast utilities
;; ---------------------------------------------------------------------------

(defun prism--relative-luminance (color)
  "Return relative luminance of COLOR per WCAG 2.1."
  (let ((rgb (prism--color-values color)))
    (when rgb
      (let ((r (prism--srgb->linear (/ (float (nth 0 rgb)) 65535)))
            (g (prism--srgb->linear (/ (float (nth 1 rgb)) 65535)))
            (b (prism--srgb->linear (/ (float (nth 2 rgb)) 65535))))
        (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))))))

(defun prism-contrast-ratio (c1 c2)
  "Return WCAG contrast ratio between colors C1 and C2."
  (let* ((l1 (prism--relative-luminance c1))
         (l2 (prism--relative-luminance c2))
         (Llight (max l1 l2))
         (Ldark  (min l1 l2)))
    (/ (+ Llight 0.05) (+ Ldark 0.05))))

;; ---------------------------------------------------------------------------
;; Core adaptation
;; ---------------------------------------------------------------------------

(defun prism--theme-is-dark-p (&optional frame)
  "Return t if the current default background is darker than L*=50."
  (let* ((bg (face-attribute 'default :background frame))
         (lch (prism-color->lch bg)))
    (< (nth 0 lch) 50)))

(defun prism--clamp-lch (L C h is-fg is-dark bg-L)
  "Clamp L* and ensure contrast against BG-L; return (L' C' h')."
  (let* ((min-L (if is-dark
                    (if is-fg prism-dark-min-fg-L prism-dark-min-bg-L)
                  (if is-fg prism-light-min-fg-L prism-light-min-bg-L)))
         (max-L (if is-dark
                    (if is-fg prism-dark-max-fg-L prism-dark-max-bg-L)
                  (if is-fg prism-light-max-fg-L prism-light-max-bg-L))))
    (setq L (max min-L (min max-L L)))
    ;; Enforce contrast against BG.
    (when bg-L
      (let* ((lighter (> L bg-L))
             (ratio-fn (lambda (fg bg)
                         (let* ((fg-rgb (prism-lch->color fg C h))
                                (bg-rgb (prism-lch->color bg 0 0)))
                           (and fg-rgb bg-rgb (prism-contrast-ratio fg-rgb bg-rgb)))))
             (ratio (funcall ratio-fn L bg-L)))
        (when ratio
          (cond
           ((< ratio prism-min-ratio)
            (let ((lo (if lighter L min-L))
                  (hi (if lighter max-L L)))
              (cl-loop repeat 20
                       do (let* ((mid (/ (+ lo hi) 2.0))
                                 (mid-ratio (funcall ratio-fn mid bg-L)))
                            (if (or (not mid-ratio) (< mid-ratio prism-min-ratio))
                                (if lighter (setq lo mid) (setq hi mid))
                              (if lighter (setq hi mid) (setq lo mid)))))
              (setq L (if lighter hi lo))))
           ((> ratio prism-max-ratio)
            (let ((lo (if lighter (max min-L bg-L) L))
                  (hi (if lighter L (min max-L bg-L))))
              (cl-loop repeat 20
                       do (let* ((mid (/ (+ lo hi) 2.0))
                                 (mid-ratio (funcall ratio-fn mid bg-L)))
                            (if (or (not mid-ratio) (> mid-ratio prism-max-ratio))
                                (if lighter (setq hi mid) (setq lo mid))
                              (if lighter (setq lo mid) (setq hi mid)))))
              (setq L (if lighter lo hi))))))))
    (list L C h)))

(defun prism--face-has-p (face attribute)
  "Return non-nil if FACE explicitly specifies ATTRIBUTE (a keyword)."
  (let ((val (face-attribute face attribute nil nil)))  ; no inherit, no default
    (and val (not (eq val 'unspecified)))))

(defun prism-adapt-face (face)
  "Adapt FACE's colors in-place.  Non-destructive: only changes when needed."
  (let* ((has-bg (prism--face-has-p face :background))
         (has-fg (prism--face-has-p face :foreground))
         (default-bg (face-attribute 'default :background nil))
         bg-str fg-str bg-lch fg-lch is-dark)
    (when (and (or has-bg has-fg)
               (not (memq face prism-preserved-faces)))
      ;; Use inherited values for the math, but only write back to faces
      ;; that explicitly set the attribute (preserving inheritance chains).
      (when has-bg
        (setq bg-str (face-attribute face :background nil t)))
      (when has-fg
        (setq fg-str (face-attribute face :foreground nil t)))
      (when bg-str
        (setq bg-lch (prism-color->lch bg-str)))
      (when fg-str
        (setq fg-lch (prism-color->lch fg-str)))
      (setq is-dark (prism--theme-is-dark-p))
      ;; Clamp background
      (when bg-lch
        (let* ((clamped (prism--clamp-lch (nth 0 bg-lch) (nth 1 bg-lch) (nth 2 bg-lch)
                                          nil is-dark nil)))
          (unless (and (= (nth 0 clamped) (nth 0 bg-lch))
                       (= (nth 1 clamped) (nth 1 bg-lch)))
            (setq bg-str (prism-lch->color (nth 0 clamped) (nth 1 clamped) (nth 2 clamped))))))
      ;; Clamp foreground against (possibly adjusted) background
      (when fg-lch
        (let ((ref-bg-L (nth 0 (prism-color->lch (if has-bg bg-str default-bg)))))
          (when ref-bg-L
            (let ((clamped (prism--clamp-lch (nth 0 fg-lch) (nth 1 fg-lch) (nth 2 fg-lch)
                                            t is-dark ref-bg-L)))
              (unless (and (= (nth 0 clamped) (nth 0 fg-lch))
                           (= (nth 1 clamped) (nth 1 fg-lch)))
                (setq fg-str (prism-lch->color (nth 0 clamped) (nth 1 clamped) (nth 2 clamped))))))))
      ;; Apply -- only for attributes the face actually specified.
      (when (and has-bg bg-str)
        (set-face-attribute face nil :background bg-str))
      (when (and has-fg fg-str)
        (set-face-attribute face nil :foreground fg-str)))))

;;;###autoload
(defun prism-soften-theme-faces (&rest _)
  "Walk all defined faces and soften contrast."
  (interactive)
  (let ((count 0))
    (dolist (face (face-list))
      (condition-case nil
          (progn
            (prism-adapt-face face)
            (cl-incf count))
        (error nil)))
    (message "prism: adjusted %d faces" count)))

;;;###autoload
(provide 'prism)
;;; prism.el ends here
