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
    (setq chatgpt-shell-models
          (mapcar (lambda (spec)
                    (chatgpt-shell-ollama-make-model
                     :version (car spec)
                     :token-width 4
                     :context-window (cdr spec)))
                  '(("gemma4:31b"                            . 32768)
                    ("qwen3.6:35b"                           . 32768)
                    ("deepseek-r1:32b"                       . 32768)
                    ("leanstral:latest"                      . 32768)
                    ("qwen3-coder:30b"                       . 32768)
                    ("qwen2.5-coder:32b"                     . 32768)
                    ("qwen2.5-coder:7b"                      . 32768)
                    ("dolphin-mixtral:8x7b-v2.5"             . 32768)
                    ;;("llama2-70b-uncensored:latest"          . 4096)
                    ;;("llama1-65b:latest"                     . 2048)
                    ("llama1-30b:latest"                     . 2048)
                    ("llama1-13b:latest"                     . 2048)
                    ("mpt-30b:latest"                        . 8192)
                    ("gpt-neox-20b:latest"                   . 2048)
                    ("pythia-12b:latest"                     . 2048)
                    ("bloom-7b1:latest"                      . 2048)
                    ("gpt4chan-8b-v2ray:latest"              . 2048)
                    ("richardyoung/bfs-prover-v2-32b:q4_K_M" . 32768)
                    ("BoltonBailey/Kimina-Prover-Distill-8B:latest" . 32768))))
    (setq chatgpt-shell-model-version "gemma4:31b")))
