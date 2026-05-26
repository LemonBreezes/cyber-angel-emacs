;;; cae/ai/+local.el -*- lexical-binding: t; -*-

;; Non-lean (general) coding models, served by Ollama on cae-ip-address:11434.
;; Each respects the 32GB VRAM budget: either fully VRAM-resident, or a low-active
;; MoE whose experts stream from RAM. (Lean-prover models are defined below.)
(defvar cae-coding-fim-model "codestral:22b"
  "Inline / fill-in-the-middle completion. Codestral-22B Q5, fully in VRAM (~90 tok/s).")
(defvar cae-coding-agent-model "devstral-small-2:24b"
  "Agentic coding + commit messages. Devstral-Small-2 24B Q6, fully in VRAM, 68% SWE-bench.")
(defvar cae-coding-reasoning-model "gpt-oss:120b"
  "Heavy reasoning on hard problems. gpt-oss-120B (5B-active MoE), experts stream from RAM (~11 tok/s).")
(defvar cae-chat-model "hf.co/unsloth/Qwen3-32B-GGUF:Q5_K_M"
  "Default interactive chatgpt-shell model (general, not a coder).")

;; Lean 4 theorem-proving models, served by the same Ollama on cae-ip-address:11434.
;; Settings come from the ollama-spec-proxy benchmarks (offload + speculative
;; decoding); the 32GB VRAM budget still holds.
(defvar cae-lean-fim-model "leanstral:latest"
  "Lean-native fill-in-the-middle. Leanstral-2603 (Mistral 128x3.9B MoE, ~5B
active), experts stream from RAM (~22 tok/s, ~1s TTFT). On-demand Lean-aware
completion -- too heavy for keystroke-by-keystroke inline.")
(defvar cae-lean-agent-model "hf.co/mradermacher/Goedel-Prover-V2-8B-GGUF:q4_K_M"
  "Interactive / agentic Lean proving. Goedel-Prover-V2-8B (Qwen3-8B, open SOTA
for its size -- ties DeepSeek-Prover-V2-671B), fully in VRAM, ~300 tok/s with the
Qwen3-0.6B speculative-decode draft.")
(defvar cae-lean-prover-model "hf.co/mradermacher/Goedel-Prover-V2-32B-GGUF:q4_K_M"
  "Hardest Lean goals. Goedel-Prover-V2-32B (Qwen3-32B, ~90% miniF2F pass@32 --
open SOTA), Q4 fully in VRAM, speculative decoding via the Qwen3-0.6B draft.")

(after! minuet
  ;; Route completion through the local Ollama FIM endpoint.  cae/ai/config.el
  ;; sets `minuet-provider' to `claude' as the global default; without overriding
  ;; it here, minuet keeps calling the Anthropic API and erroring out with plz
  ;; HTTP errors (there is no ANTHROPIC_API_KEY), so completion never works.
  (setq minuet-provider 'openai-fim-compatible)
  (plist-put minuet-openai-fim-compatible-options :name "Ollama")
  (plist-put minuet-openai-fim-compatible-options :end-point
             (format "http://%s:11434/v1/completions"
                     cae-ip-address))
  (plist-put minuet-openai-fim-compatible-options :model cae-coding-fim-model)
  ;; Ollama needs no API key, but minuet's availability check still requires
  ;; `:api-key' to name a non-empty env var (the inherited default points at the
  ;; unset DEEPSEEK_API_KEY).  TERM is always set -- this is minuet's own
  ;; documented workaround for Ollama.
  (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
  ;; Default (3s) is too short for a cold 22B FIM load into VRAM; give the first
  ;; request room before it warms up.
  (setq minuet-request-timeout 8))

(after! llm
  (require 'llm-ollama)
  (setq magit-gptcommit-llm-provider
        (make-llm-ollama
         :host cae-ip-address
         :port 11434
         :chat-model cae-coding-agent-model)))

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

(after! aidermacs
  ;; Point aider at the local Ollama server.  The spawned aider child inherits
  ;; OLLAMA_API_BASE from `process-environment', which is how litellm (aider's
  ;; backend) discovers the Ollama endpoint when a model uses the
  ;; `ollama_chat/' prefix.
  (setenv "OLLAMA_API_BASE" (format "http://%s:11434" cae-ip-address))
  ;; Aider caps Ollama's context at 2k tokens by default -- way too small for
  ;; agentic coding.  Both devstral-small-2 and gpt-oss support ~128k; 32k is
  ;; a balanced default (bump if you want longer context at the cost of VRAM
  ;; and TTFT).
  (setenv "OLLAMA_CONTEXT_LENGTH" "32768")
  ;; Use local models for every aider role.  Architect mode pairs the heavy
  ;; reasoner (planner) with the fast in-VRAM coder (applies the edits).  The
  ;; weak model handles cheap chores like commit messages -- devstral is fine
  ;; there since it's already loaded.
  (setq aidermacs-use-architect-mode t
        aidermacs-default-model   (concat "ollama_chat/" cae-coding-agent-model)
        aidermacs-architect-model (concat "ollama_chat/" cae-coding-reasoning-model)
        aidermacs-editor-model    (concat "ollama_chat/" cae-coding-agent-model)
        aidermacs-weak-model      (concat "ollama_chat/" cae-coding-agent-model))
  ;; Override config.el's extra-args: drop `--cache-prompts' and
  ;; `--cache-keepalive-pings' (Anthropic prompt-caching, no-op + noisy warning
  ;; on Ollama) and keep only the provider-agnostic flags.
  (setq aidermacs-extra-args
        '("--watch-files"
          "--auto-accept-architect"
          "--chat-language" "English")))

(use-package! pi-coding-agent
  :defer t :config
  (after! pi-coding-agent-ui
    (setq pi-coding-agent-input-markdown-highlighting t))
  (when (modulep! :editor evil)
    (after! evil
      (map! :map pi-coding-agent-input-mode-map
            :n "RET" #'pi-coding-agent-send
            :n "<return>" #'pi-coding-agent-send
            :n "ZQ" #'pi-coding-agent-quit
            :n "<f5>" #'pi-coding-agent-menu
            :map pi-coding-agent-chat-mode-map
            :n "q" #'pi-coding-agent-quit
            :n "ZQ" #'pi-coding-agent-quit
            :n "<f5>" #'pi-coding-agent-menu))))
