;;; cae/ai/+local.el -*- lexical-binding: t; -*-

;; Non-lean (general) coding models, served by Ollama on cae-ip-address:11434.
;; Each respects the 32GB VRAM budget: either fully VRAM-resident, or a low-active
;; MoE whose experts stream from RAM. (Lean-prover models live elsewhere.)
(defvar cae-coding-fim-model "codestral:22b"
  "Inline / fill-in-the-middle completion. Codestral-22B Q5, fully in VRAM (~90 tok/s).")
(defvar cae-coding-agent-model "devstral-small-2:24b"
  "Agentic coding + commit messages. Devstral-Small-2 24B Q6, fully in VRAM, 68% SWE-bench.")
(defvar cae-coding-reasoning-model "gpt-oss:120b"
  "Heavy reasoning on hard problems. gpt-oss-120B (5B-active MoE), experts stream from RAM (~11 tok/s).")
(defvar cae-chat-model "hf.co/unsloth/Qwen3-32B-GGUF:Q5_K_M"
  "Default interactive chatgpt-shell model (general, not a coder).")

(after! minuet
  (plist-put minuet-openai-fim-compatible-options :name "Ollama")
  (plist-put minuet-openai-fim-compatible-options :end-point
             (format "http://%s:11434/v1/completions"
                     cae-ip-address))
  (plist-put minuet-openai-fim-compatible-options :model cae-coding-fim-model))

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
    (setq chatgpt-shell-model-version cae-chat-model)))
