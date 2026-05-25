;;; cae/ai/+local.el -*- lexical-binding: t; -*-

(after! minuet
  (plist-put minuet-openai-fim-compatible-options :name "Ollama")
  (plist-put minuet-openai-fim-compatible-options :end-point
             (format "http://%s:11434/v1/completions"
                     cae-ip-address))
  (defvar cae-minuet-fim-model "qwen2.5-coder:7b")
  (plist-put minuet-openai-fim-compatible-options :model cae-minuet-fim-model-small))

(after! llm
  (require 'llm-ollama)
  (setq magit-gptcommit-llm-provider
        (make-llm-ollama
         :host cae-ip-address
         :port 11434
         :chat-model "qwen3-coder:30b")))

(after! chatgpt-shell
  (when (bound-and-true-p cae-ip-address)
    (setq chatgpt-shell-models nil)
    (require 'chatgpt-shell-ollama)
    ;; Some Ollama models (e.g. HF GGUF imports like Midnight-Miqu) report a
    ;; `quantization_level' of "unknown", which
    ;; `chatgpt-shell-ollama--parse-token-width' cannot parse, so it returns
    ;; nil.  `chatgpt-shell-ollama-make-model' then signals "Missing mandatory
    ;; :token-width param", and since `chatgpt-shell-ollama-load-models' fetches
    ;; every model with a single `mapcar', that one error aborts loading for
    ;; *all* Ollama models.  Default the width to 4 (matching the built-in
    ;; models) whenever it cannot be parsed.
    (cae-defadvice! cae-ai-ollama-default-token-width-a (width)
      :filter-return #'chatgpt-shell-ollama--parse-token-width
      (or width 4))
    (setq chatgpt-shell-ollama-api-url-base
          (format "http://%s:11434" cae-ip-address))
    (chatgpt-shell-ollama-load-models)
    (setq chatgpt-shell-model-version "hf.co/unsloth/Qwen3-32B-GGUF:Q5_K_M")))
