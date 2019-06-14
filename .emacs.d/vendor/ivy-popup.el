;;; Code:
(require 'ivy)

(defcustom ivy-popup-direction 'above
  "Default direction to popup window."
  :group 'ivy
  :type '(choice (const :tag "Above" 'above)
                 (const :tag "Below" 'below)))

(defvar *ivy-popup-buffer* nil)
(defvar *ivy-popup-window* nil)

(defun ivy-display-function-window (text)
  (progn
    (unless (window-live-p *ivy-popup-window*)
      (setq *ivy-popup-buffer* (get-buffer-create "*ivy-popup*"))
      (setq *ivy-popup-window*
            (split-window (minibuffer-selected-window)
                          (- ivy-height 1)
                          ivy-popup-direction))
      (window--display-buffer *ivy-popup-buffer* *ivy-popup-window* nil))

    (with-current-buffer *ivy-popup-buffer*
      (setq-local truncate-lines t)

      (let ((buffer-undo-list t)
            (inhibit-read-only t))
        (delete-region (point-min) (point-max))
        (ivy--cleanup)
        (ivy--insert-prompt)
        (insert text))

      (fit-window-to-buffer *ivy-popup-window* (+ 2 ivy-height) 2))))

(provide 'ivy-popup)
;;; ivy-popup.el ends here
