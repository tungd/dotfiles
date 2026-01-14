;;; gptel-zai.el --- GLM model integration for gptel -*- lexical-binding: t -*-

;; Author: Tung Dao
;; Keywords: ai, gptel, glm
;; Package-Requires: ((gptel "0.8.0"))

;;; Commentary:

;; This package provides GLM model integration for gptel, connecting to the
;; Zai API endpoint.

;;; Code:

(require 'gptel-openai)

;;;###autoload
(defun gptel-zai-setup ()
  "Set up gptel to use the GLM backend from Zai API."
  (setopt gptel-backend
          (gptel-make-openai
              "GLM"
            :stream t
            :host "api.z.ai"
            :endpoint "/api/coding/paas/v4/chat/completions"
            :models '((glm-4.7
                       :description "General Language Model 4.7 (latest)"
                       :capabilities (media tool-use cache)
                       :context-window 128
                       :input-cost 0.6
                       :output-cost 2.2
                       :cutoff-date "2025-01")
                      (glm-4.6
                       :description "General Language Model 4.6"
                       :capabilities (media tool-use cache)
                       :context-window 128
                       :input-cost 0.6
                       :output-cost 2.2
                       :cutoff-date "2025-01")
                      (glm-4.5-air
                       :description "General Language Model 4.5 Air"
                       :capabilities (media tool-use cache)
                       :context-window 128
                       :input-cost 0.2
                       :output-cost 1.1
                       :cutoff-date "2024-12")
                      (glm-4.5-x
                       :description "General Language Model 4.5 (fast response)"
                       :capabilities (media tool-use cache)
                       :context-window 128
                       :input-cost 2.2
                       :output-cost 8.9
                       :cutoff-date "2024-12")
                      (glm-4.5-air-x
                       :description "General Language Model 4.5 Air (fast response)"
                       :capabilities (media tool-use cache)
                       :context-window 128
                       :input-cost 1.1
                       :output-cost 4.5
                       :cutoff-date "2024-12"))
            :key (auth-source-pick-first-password :host "api.z.ai" :max 1))))

(provide 'gptel-zai)
;;; gptel-zai.el ends here
