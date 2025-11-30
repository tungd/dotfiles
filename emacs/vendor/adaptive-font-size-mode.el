;;; adaptive-font-size-mode.el --- Adaptive font size -*- lexical-binding: t; -*-

;;;###autoload
(defun adaptive-font-size-adapt (&optional frame)
  "Set font size based on monitor PPI for FRAME (or current frame)."
  (let* ((frame (or frame (selected-frame)))
         (attrs (frame-monitor-attributes frame))
         (size (alist-get 'mm-size attrs))
         (geometry (alist-get 'geometry attrs))
         (ppi (/ (caddr geometry) (/ (car size) 25.4))))
    (if (> ppi 120)
        (set-face-attribute 'default frame :height 160)
      (set-face-attribute 'default frame :height 150))))

;;;###autoload
(define-minor-mode adaptive-font-size-mode
  "Toggle Adaptive Font Mode.

When enabled, this global minor mode adjusts the default font
size based on the screen's PPI when focus changes or a new frame
is created."
  :global t
  :group 'display
  :lighter " AFont"

  ;; Code to run when the mode is enabled
  (if adaptive-font-size-mode
      (progn
        ;; Add the function to the hooks
        (add-function :after after-focus-change-function #'adaptive-font-size-adapt)
        (add-hook 'after-make-frame-functions #'adaptive-font-size-adapt)
        ;; Run it once for the current frame
        (adaptive-font-size-adapt))

    ;; Code to run when the mode is disabled
    (progn
      ;; Remove the function from the hooks
      (remove-function after-focus-change-function #'adaptive-font-size-adapt)
      (remove-hook 'after-make-frame-functions #'adaptive-font-size-adapt))))

(provide 'adaptive-font-size-mode)
