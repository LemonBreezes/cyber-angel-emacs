;;; cae/ai/+local.el -*- lexical-binding: t; -*-

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
  (setq chatgpt-shell-models
        (mapcar (lambda (spec)
                  (chatgpt-shell-ollama-make-model
                   :version (car spec)
                   :token-width 4
                   :context-window (cdr spec)))
                '(("gemma4:31b"        . 32768)
                  ("qwen3.6:35b"       . 32768)
                  ("deepseek-r1:32b"   . 32768)
                  ("leanstral:latest"  . 32768)
                  ("qwen3-coder:30b"   . 32768)
                  ("qwen2.5-coder:32b" . 32768)
                  ("qwen2.5-coder:7b"  . 32768))))
  (setq chatgpt-shell-model-version "gemma4:31b"))
