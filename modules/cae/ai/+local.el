;;; cae/ai/+local.el -*- lexical-binding: t; -*-

(when (bound-and-true-p cae-ip-address)
  (after! minuet
    (plist-put minuet-openai-fim-compatible-options :name "Ollama")
    (plist-put minuet-openai-fim-compatible-options :end-point
               (format "http://%s:11434/v1/completions"
                       cae-ip-address))
    (defvar cae-minuet-fim-model-small "qwen2.5-coder:7b"
      "Smaller FIM model that fits alongside a 30B+ chat model in 32 GB VRAM.")
    (defvar cae-minuet-fim-model-large "qwen2.5-coder:32b"
      "Larger FIM model — best quality, use solo on the 5090.")
    (plist-put minuet-openai-fim-compatible-options :model cae-minuet-fim-model-small))

  (after! llm
    (require 'llm-ollama)
    (setq magit-gptcommit-llm-provider
          (make-llm-ollama
           :host cae-ip-address
           :port 11434
           :chat-model "qwen3-coder:30b")))

  (after! chatgpt-shell
    (require 'chatgpt-shell-ollama)
    (setq chatgpt-shell-ollama-api-url-base
          (format "http://%s:11434" cae-ip-address))
    (setq chatgpt-shell-model-version "gemma4:31b")))
