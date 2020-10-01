(require 'cc-mode)

(defvar mql-font-lock-keywords
  '(input))

(defvar mql-mode-hook nil)

(defvar mql-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for MQL major mode")

;;;###autoload
(define-derived-mode mql-mode c-mode "MQL"
  "Major mode for editing MetaTrader MQL4/5 code."
  (setq font-lock-defaults '((mql-font-lock-keywords))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mq4\\'" . mql-mode))

(provide 'mql-mode)
