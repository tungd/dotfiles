;;; td-completing-read.el --- Hybrid async completing-read  -*- lexical-binding: t; -*-

(defgroup td-completing-read nil
  "Hybrid async completing-read with finite initial display."
  :group 'minibuffer)

(defcustom td-cr-phase-1-size 200
  "Max candidates to return in Phase 1 (sync initial display).
The full candidate list is computed asynchronously after display."
  :type 'integer)

(defun td--make-hybrid-table (collection)
  "Wrap COLLECTION in a hybrid completion table.
Phase 1 returns a capped, prescient-sorted subset for instant display."
  (lambda (string pred action)
    (pcase action
      ('metadata
       (let* ((orig (if (functionp collection)
                        (funcall collection string pred action)
                      nil))
              (copy (copy-sequence (cdr orig))))
         (setq copy (assq-delete-all 'category copy))
         `(metadata
           (eager-display . t)
           (eager-update . t)
           (cycle . t)
           ,@copy)))
      (`t                        ; all-completions
       (let* ((all (complete-with-action t collection string pred))
              (filtered (if (fboundp 'prescient-filter)
                            (prescient-filter string all)
                          all))
              (sorted (if (fboundp 'prescient-completion-sort)
                          (prescient-completion-sort filtered)
                        filtered)))
         (seq-take sorted td-cr-phase-1-size)))
      (_                          ; try-completion, test-completion, boundaries
       (complete-with-action action collection string pred)))))

(defun td-completing-read (prompt collection &optional predicate
                                  require-match initial-input
                                  hist def inherit-input-method)
  "`completing-read' using a hybrid completion table.
Delegates to `completing-read-default' with a wrapped collection
that limits the initial candidate set."
  (apply #'completing-read-default prompt
         (td--make-hybrid-table collection)
         predicate require-match initial-input hist def
         inherit-input-method))

;;;###autoload
(define-minor-mode td-completing-read-mode
  "Toggle hybrid async completing-read.
When active, `completing-read' uses a hybrid table that returns a
limited prescient-sorted subset immediately, then fills in the
full list asynchronously."
  :global t
  :group 'td-completing-read
  (if td-completing-read-mode
      (setq completing-read-function #'td-completing-read)
    (setq completing-read-function #'completing-read-default)))

(provide 'td-completing-read)
;;; td-completing-read.el ends here
