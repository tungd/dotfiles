;;; auto-dark-mode.el --- Auto switch theme based on system appearance -*- lexical-binding: t; -*-

(defgroup auto-dark-mode nil
  "Automatically switch themes based on system dark/light mode."
  :group 'faces)

(defcustom auto-dark-dark-theme 'modus-vivendi
  "Theme to use when system is in dark mode."
  :type 'symbol
  :group 'auto-dark-mode)

(defcustom auto-dark-light-theme 'modus-operandi
  "Theme to use when system is in light mode."
  :type 'symbol
  :group 'auto-dark-mode)

(defvar auto-dark--in-progress nil
  "Guard variable to prevent recursive calls to auto-dark-adapt.")

(defun auto-dark--detect-system-appearance ()
  "Detect system appearance using AppleScript.
Returns 'dark or 'light."
  (let* ((script "tell application \"System Events\"
    tell appearance preferences
      return dark mode
    end tell
  end tell")
         (result (shell-command-to-string (concat "osascript -e '" script "'")))
         (dark-mode-p (string-trim result)))
    (if (string= dark-mode-p "true")
        'dark
      'light)))

(defun auto-dark--apply-theme (appearance)
  "Apply the appropriate theme based on APPEARANCE ('dark or 'light)."
  (cond
   ((eq appearance 'dark)
    (setq ns-appearance 'dark)
    (load-theme auto-dark-dark-theme t t))
   ((eq appearance 'light)
    (setq ns-appearance 'light)
    (load-theme auto-dark-light-theme t t))))

;;;###autoload
(defun auto-dark-adapt (&optional frame)
  "Detect system appearance and switch theme accordingly for FRAME (or current frame)."
  (unless auto-dark--in-progress
    (let ((auto-dark--in-progress t))
      (let ((appearance (auto-dark--detect-system-appearance)))
        (auto-dark--apply-theme appearance)))))

;;;###autoload
(define-minor-mode auto-dark-mode
  "Toggle Auto Dark Mode.

When enabled, this global minor mode detects the system's
dark/light mode setting and switches the Emacs theme
accordingly when focus changes."
  :global t
  :group 'auto-dark-mode
  :lighter " AutoDark"

  ;; Code to run when the mode is enabled
  (if auto-dark-mode
      (progn
        ;; Add the function to the hooks
        (add-function :after after-focus-change-function #'auto-dark-adapt)
        (add-hook 'after-make-frame-functions #'auto-dark-adapt)
        ;; Run it once for the current frame
        (let ((auto-dark--in-progress nil))
          (auto-dark-adapt)))

    ;; Code to run when the mode is disabled
    (progn
      ;; Remove the function from the hooks
      (remove-function after-focus-change-function #'auto-dark-adapt)
      (remove-hook 'after-make-frame-functions #'auto-dark-adapt))))

(provide 'auto-dark-mode)
