
;;; Code:
(require 'ivy)

(defcustom ivy-popup-direction 'above
  "Default direction to popup window."
  :group 'ivy)

(defvar *ivy-popup-buffer* nil)
(defvar *ivy-popup-window* nil)

(defun ivy--insert-minibuffer (text)
  "Display the TEXT in the popup window instead of minibuffer."
  (let ((resize-mini-windows nil)
        (update-fn (ivy-state-update-fn ivy-last))
        (inhibit-read-only t)
        (input (minibuffer-contents))
        deactivate-mark)

    (unless (window-live-p *ivy-popup-window*)
      ;; New completion -> popup
      (setq *ivy-popup-buffer* (get-buffer-create "*ivy-popup*"))
      (setq *ivy-popup-window*
            (split-window (minibuffer-selected-window) nil ivy-popup-direction))
      (window--display-buffer *ivy-popup-buffer* *ivy-popup-window* nil))

    (with-current-buffer *ivy-popup-buffer*
      (ivy--cleanup)
      (delete-region (point-min) (point-max))

      (when update-fn
        (funcall update-fn))

      (insert input)
      (ivy--insert-prompt)
      ;; Do nothing if while-no-input was aborted.
      (when (stringp text)
        (let ((buffer-undo-list t))
          (save-excursion
            (forward-line 1)
            (insert text))))

      ;; (when (display-graphic-p)
      ;;   (ivy--resize-minibuffer-to-fit))
      (fit-window-to-buffer *ivy-popup-window* (+ 4 ivy-height) 2 20 20))))

(provide 'ivy-popup)
;;; ivy-popup.el ends here
