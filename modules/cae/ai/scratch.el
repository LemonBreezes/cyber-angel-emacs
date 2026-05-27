;;; cae/ai/scratch.el -*- lexical-binding: t; -*-

;; For evaluating interactively when I want to cross-check AI output.

(setq aidermacs-default-model "openrouter/google/gemini-3.1-pro-preview")
(setq aidermacs-editor-model "anthropic/claude-opus-4-7")
(setq aidermacs-auto-commits nil)
(setq aidermacs-backend 'comint)
;; Override config.el's extra-args: drop `--cache-prompts' and
;; `--cache-keepalive-pings' (Anthropic prompt-caching, no-op + noisy warning
;; on Ollama) and keep only the provider-agnostic flags.
(setq aidermacs-extra-args
      '("--watch-files"
        "--auto-accept-architect"
        "--chat-language" "English"))

