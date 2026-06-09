;;; gptel-cloudflare-ai-gateway.el --- Cloudflare AI Gateway integration for gptel -*- lexical-binding: t -*-

;; Author: Tung Dao
;; Keywords: ai, gptel, cloudflare, workers-ai, gemma
;; Package-Requires: ((gptel "0.8.0"))

;;; Commentary:

;; This package configures gptel for Cloudflare AI Gateway's
;; OpenAI-compatible chat completions endpoint.

;;; Code:

(require 'gptel-openai)

(defconst gptel-cloudflare-ai-gateway-host "gateway.ai.cloudflare.com")

(defconst gptel-cloudflare-ai-gateway-endpoint
  "/v1/d1c7d16503218b2f1fa50a660f8c887d/td/compat/chat/completions")

(defconst gptel-cloudflare-ai-gateway-models
  '((dynamic/quick
     :description "Dynamic router quick response model"
     :capabilities (media tool-use reasoning json)
     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
     :context-window 1000
     :input-cost 0.00
     :output-cost 0.05)))

;;;###autoload
(defun gptel-cloudflare-ai-gateway-api-key ()
  "Return the Cloudflare AI Gateway API key from auth-source."
  (auth-source-pick-first-password
   :host gptel-cloudflare-ai-gateway-host
   :max 1))

(defun gptel-cloudflare-ai-gateway--make-backend (name request-params)
  "Create a Cloudflare AI Gateway gptel backend named NAME.

REQUEST-PARAMS are sent with every request using that backend."
  (gptel-make-openai
      name
    :stream t
    :host gptel-cloudflare-ai-gateway-host
    :endpoint gptel-cloudflare-ai-gateway-endpoint
    :header (lambda (_info)
              (when-let* ((key (gptel--get-api-key)))
                `(("cf-aig-authorization" . ,(concat "Bearer " key)))))
    :models gptel-cloudflare-ai-gateway-models
    :request-params request-params
    :key #'gptel-cloudflare-ai-gateway-api-key))

;;;###autoload
(defun gptel-cloudflare-ai-gateway-setup ()
  "Set up gptel to use Cloudflare AI Gateway."
  (setopt
   gptel-backend
   (gptel-cloudflare-ai-gateway--make-backend
    "Cloudflare AI Gateway"
    '(:chat_template_kwargs (:enable_thinking :json-false))))
  (gptel-cloudflare-ai-gateway--make-backend
   "Cloudflare AI Gateway Low Reasoning"
   '(:reasoning_effort "low"
     :chat_template_kwargs (:enable_thinking t)))
  (gptel-cloudflare-ai-gateway--make-backend
   "Cloudflare AI Gateway Medium Reasoning"
   '(:reasoning_effort "medium"
     :chat_template_kwargs (:enable_thinking t))))

(provide 'gptel-cloudflare-ai-gateway)
;;; gptel-cloudflare-ai-gateway.el ends here
