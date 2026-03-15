;;; gptel-alibaba-coding-plan.el --- Alibaba Coding Plan integration for gptel -*- lexical-binding: t -*-

;; Author: Tung Dao
;; Keywords: ai, gptel, qwen, glm, kimi, minimax
;; Package-Requires: ((gptel "0.8.0"))

;;; Commentary:

;; This package provides Alibaba Coding Plan integration for gptel via the
;; OpenAI-compatible DashScope endpoint.

;;; Code:

(require 'gptel-openai)

;;;###autoload
(defun gptel-alibaba-coding-plan-setup ()
  "Set up gptel to use Alibaba Coding Plan."
  (setopt
   gptel-backend
   (gptel-make-openai
       "Alibaba Coding Plan"
     :stream t
     :host "coding-intl.dashscope.aliyuncs.com"
     :endpoint "/v1/chat/completions"
     :models '((qwen3.5-plus
                :description "Qwen3.5-Plus"
                :capabilities (media tool-use)
                :context-window 1000000)
               (qwen3-max-2026-01-23
                :description "Qwen3-Max snapshot 2026-01-23"
                :capabilities (tool-use)
                :context-window 262144)
               (qwen3-coder-next
                :description "Qwen3 Coder Next"
                :capabilities (tool-use)
                :context-window 262144)
               (qwen3-coder-plus
                :description "Qwen3 Coder Plus"
                :capabilities (tool-use)
                :context-window 1000000)
               (glm-5
                :description "GLM-5"
                :capabilities (tool-use)
                :context-window 202752)
               (glm-4.7
                :description "GLM-4.7"
                :capabilities (tool-use)
                :context-window 169984)
               (kimi-k2.5
                :description "Kimi K2.5"
                :capabilities (media tool-use)
                :context-window 262144)
               (MiniMax-M2.5
                :description "MiniMax M2.5"
                :capabilities (tool-use)
                :context-window 196608))
     :key (auth-source-pick-first-password :host "alibaba" :max 1))))

(provide 'gptel-alibaba-coding-plan)
;;; gptel-alibaba-coding-plan.el ends here
