(require 'ob)
(require 'graphql-mode)
(require 'request)
(require 'json)

(defconst org-babel-header-args:grahql
  '((endpoint . :string)
    (mode . :string))
  "graphql header arguments")

(defgroup ob-graphql nil
  "org-mode blocks for http request"
  :group 'org)

(defun ob-graphql-pretty-print-buffer ()
  (json-pretty-print-buffer)
  (buffer-string))

(defun org-babel-execute:graphql (body params)
  (let ((graphql-url (cdr (assoc :endpoint params)))
        (mode (or (cdr (assoc :mode params)) "query")))
    (cond ((string-equal mode "json")
           (graphql--query body))
          ((string-equal mode "query")
           (request-response-data
            (request graphql-url
                     :sync t
                     :params `(("query" . ,body))
                     :parser 'ob-graphql-pretty-print-buffer))))))

(define-derived-mode ob-graphql-mode graphql-mode "ob graphql")

(eval-after-load "org"
  '(add-to-list 'org-src-lang-modes '("graphql" . "ob-graphql")))

(provide 'ob-graphql)
