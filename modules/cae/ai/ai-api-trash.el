;;; cae/ai/ai-api-trash.el -*- lexical-binding: t; -*-

;; This file is so that I have examples of how to set up models.

(with-eval-after-load 'minuet
  (setq minuet-provider 'openai)
  (plist-put minuet-openai-options :model "o3-mini")
  ;; max_tokens and top_p parameter is not available for o3-mini model,
  ;; so remember to remove these two options if you have set them previously
  (minuet-set-optional-options minuet-openai-options :max_tokens nil)
  (minuet-set-optional-options minuet-openai-options :top_p nil)
  (minuet-set-optional-options minuet-openai-options :reasoning_effort "low"))

(after! minuet
  (setq minuet-openai-fim-compatible-options
        '(:end-point "https://integrate.api.nvidia.com/v1/chat/completions"
          :api-key cae-secrets-get-nvidia-api-key
          :model "deepseek-ai/deepseek-r1"
          :name "Deepseek"
          :template (:prompt minuet--default-fim-prompt-function
                     :suffix minuet--default-fim-suffix-function)
          :optional nil))
  (setq minuet-provider 'openai-fim-compatible))
