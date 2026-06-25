;;; tterm-ibuffer.el --- Ibuffer integration for tterm -*- lexical-binding: t; -*-

;; Optional ibuffer column for live tterm buffers.

(require 'ibuffer)
(require 'tterm)

(define-ibuffer-column tterm-status
  (:name "tterm")
  (if (eq (buffer-local-value 'major-mode buffer) 'tterm-mode)
      (tterm-osc-status-string buffer)
    ""))

(provide 'tterm-ibuffer)
;;; tterm-ibuffer.el ends here
