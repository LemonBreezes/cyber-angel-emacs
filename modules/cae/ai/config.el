;;; cae/ai/config.el -*- lexical-binding: t; -*-

(require 'cae-lib)

(defvar cae-ai-chatgpt-shell-workspace-name "*chatgpt*")
(defvar cae-ai-dall-e-shell-workspace-name "*dall-e*")

;;; Set the models

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

;; Each coding role uses the model trained for that role -- collapsing them all
;; onto `cae-chat-model` was a mistake (a Qwen3-VL chat model hallucinates Lean-3
;; commands when asked to scaffold Lean 4, because it's a vision-language tune).
;; Keep:
;;   - cae-coding-fim-model      = codestral:22b   (vLLM-coder + suffix decode)
;;   - cae-coding-agent-model    = devstral-small-2:24b   (SWE-bench-tuned)
;;   - cae-coding-reasoning-model= gpt-oss:120b    (heavy reasoning)
;;   - cae-chat-model            = a general chat model, separate
;; To swap the agent to the fast A3B coder, set:
;;   (setq cae-coding-agent-model "qwen3-coder:30b")  ; ~276 tok/s, tool-trained

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

;; Due to VRAM limitations, I use one model for everything currently.
(setq cae-coding-fim-model cae-chat-model
      cae-coding-agent-model cae-chat-model
      cae-coding-reasoning-model cae-chat-model)

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

(after! magit-gptcommit
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
  ;; Aider talks to the local stack through litellm's openai-compat path
  ;; (`openai/<model>' + --api-base pointing at the proxy's /v1).  The native
  ;; `ollama_chat/' provider in the bundled litellm hangs on streaming (NDJSON
  ;; iterator never yields chunks, even though /api/chat works fine via curl)
  ;; -- the openai/ path streams cleanly through ollama-spec-proxy's
  ;; /v1/chat/completions instead.
  ;;
  ;; We pass --api-base / --api-key as explicit aider flags rather than env
  ;; vars: `setenv' in Emacs only propagates to subprocesses spawned by THIS
  ;; Emacs (subtle when aidermacs uses comint), and a stale `OPENAI_API_BASE'
  ;; or no `OPENAI_API_BASE' silently falls back to api.openai.com and fails
  ;; with "Incorrect API key provided: ollama".  Flags can't be ignored.
  (setq cae-aidermacs--api-base   (format "http://%s:11434/v1" cae-ip-address)
        cae-aidermacs--api-key    "ollama")
  ;; Aider caps Ollama's context at 2k tokens by default -- way too small for
  ;; agentic coding.  Both devstral-small-2 and gpt-oss support ~128k; 32k is
  ;; a balanced default (bump if you want longer context at the cost of VRAM
  ;; and TTFT).  Still read by ollama-server, so keep it.
  (setenv "OLLAMA_CONTEXT_LENGTH" "32768")
  ;; Use local models for every aider role.  Architect mode pairs the heavy
  ;; reasoner (planner) with the fast in-VRAM coder (applies the edits).  The
  ;; weak model handles cheap chores like commit messages -- devstral is fine
  ;; there since it's already loaded.
  (setq aidermacs-use-architect-mode t
        aidermacs-default-model   (concat "openai/" cae-coding-agent-model)
        aidermacs-architect-model (concat "openai/" cae-coding-reasoning-model)
        aidermacs-editor-model    (concat "openai/" cae-coding-agent-model)
        aidermacs-weak-model      (concat "openai/" cae-coding-agent-model))
  ;; Set extra-args here (in `after!') instead of inside `use-package! :config'.
  ;; `:config' only runs once per package load; on `doom/reload' it does NOT
  ;; re-run, so edits to these args would silently not take effect until you
  ;; restarted Emacs.  Putting it in `after!' makes a Doom reload pick it up.
  ;; --openai-api-base / --openai-api-key wire the openai-compat provider at
  ;; the local proxy; --model-metadata-file declares real context windows.
  (setq aidermacs-extra-args
        `("--openai-api-base" ,cae-aidermacs--api-base
          "--openai-api-key"  ,cae-aidermacs--api-key
          "--model-metadata-file"
          ,(expand-file-name "modules/cae/ai/aider-model-metadata.json"
                             doom-user-dir)
          "--watch-files"
          "--auto-accept-architect"
          "--chat-language" "English")))

;;; Configure the packages
(use-package! aidermacs
  :defer t :init
  (autoload 'aidermacs-transient-menu "aidermacs" nil t)
  :config
  (setq aidermacs-auto-commits nil)
  (setq aidermacs-backend 'comint)
  (cae-defadvice! cae-aidermacs-run-make-real-buffer-a ()
    :after #'aidermacs-run
    (when-let ((buf (get-buffer (aidermacs-buffer-name)))
               (_ (buffer-live-p buf)))
      (doom-set-buffer-real buf t))))


(cae-defadvice! cae-magit-gptcommit-save-buffer-a ()
  :after #'magit-gptcommit-commit-accept
  (when-let ((buf (magit-commit-message-buffer)))
    (with-current-buffer buf (save-buffer))))
(use-package! magit-gptcommit
  :after magit :init
  :config
  ;; Strict subject-only output: tool-tuned models (Devstral) tend to wrap the
  ;; subject in preamble + markdown bold, which breaks magit-gptcommit's first-
  ;; line-is-subject contract. Be emphatic; no body, no markdown, no surround.
  (setq magit-gptcommit-prompt
        "You write Git commit subject lines.

Choose ONE label from: build, chore, ci, docs, feat, fix, perf, refactor, style, test.
Labels: build=build system/deps, chore=routine/dep/license/repo upkeep, ci=CI config,
docs=docs-only, feat=new feature, fix=bug fix, perf=perf improvement (no behavior change),
refactor=neither fix nor feat, style=formatting/whitespace, test=test changes.

REPLY WITH EXACTLY ONE LINE in the form:  label: summary
- At most 50 characters total.
- Imperative tense (\"Add logging\", not \"Added logging\").
- No trailing period.
- No preamble, no explanation, no body, no markdown, no backticks, no square brackets,
  no bold markers, no quotes. Output only the single line and nothing else.

THE FILE DIFFS:
```
%s
```

One line, label: summary, now:")
  (setq magit-gptcommit-prompt-one-line magit-gptcommit-prompt)
  (magit-gptcommit-mode 1)
  (magit-gptcommit-status-buffer-setup))
(after! git-commit
  (map! :map git-commit-mode-map
        "C-c C-g" #'magit-gptcommit-commit-accept))

;; Not really a fan of this package yet.
(use-package! minuet
  :defer t :init
  ;; With my GPU, Minuet is too slow for auto-suggestions becaus it has to load
  ;; the model into VRAM.
  (when (modulep! +fim)
    (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
    (add-hook 'text-mode-hook #'minuet-auto-suggestion-mode)
    (add-hook 'conf-mode-hook #'minuet-auto-suggestion-mode))
  :config
  (map! :map minuet-active-mode-map
        :ig "M-p" #'minuet-previous-suggestion ;; invoke completion or cycle to next completion
        :ig "M-n" #'minuet-next-suggestion ;; invoke completion or cycle to previous completion
        :ig "C-f" #'minuet-accept-suggestion ;; accept whole completion
        :ig "M-f" #'cae-ai-minuet-accept-suggestion-word ;; accept one word
        :ig "C-e" #'minuet-accept-suggestion-line)
  (defun cae-ai-minuet-between-word-chars-p ()
    "Return non-nil when point is not at a word boundary."
    (and (eq (char-syntax (or (char-before) ?\s)) ?w)
         (eq (char-syntax (or (char-after)  ?\s)) ?w)))
  (add-to-list 'minuet-auto-suggestion-block-predicates
               #'cae-ai-minuet-between-word-chars-p)
  (when (modulep! :completion corfu)
    (setq minuet-auto-suggestion-debounce-delay 0.2))
  (add-hook! 'doom-escape-hook :depth -1
    (defun cae-minuet-dismiss-suggestion-h ()
      (when minuet--current-overlay
        (minuet-dismiss-suggestion)
        t)))
  (add-hook 'evil-insert-state-exit-hook #'minuet-dismiss-suggestion)
  (when (modulep! :editor evil)
    (add-hook 'minuet-active-mode-hook #'evil-normalize-keymaps)))

(use-package! chatgpt-shell
  :defer t :init
  (map! :leader
        :prefix "o"
        :desc "Toggle ChatGPT popup" "c" #'cae-ai-toggle-chatgpt-shell
        :desc "Open ChatGPT here" "C" #'chatgpt-shell
        :desc "Open ChatGPT workspace" "C-c" #'cae-ai-open-chatgpt-workspace)
  :config
  (map! :map chatgpt-shell-mode-map
        :n "RET" #'comint-send-input
        :i "RET" #'newline)
  (setq chatgpt-shell-openrouter-key (getenv "OPENROUTER_API_KEY"))
  (unless (executable-find "dwdiff")
    (warn "dwdiff is not installed, so ChatGPT shell will not be able to use it."))
  (setq chatgpt-shell-display-function #'switch-to-buffer)
  ;; Trying to stop some escape codes from showing up in my ChatGPT shell.
  (setq-hook! 'chatgpt-shell-mode-hook
    comint-process-echoes t)
  (cae-defadvice! cae-ai-ignore-ld-library-path-a (oldfun &rest args)
    :around #'shell-maker-async-shell-command
    ;; This is a hack to prevent the ChatGPT shell from inheriting
    ;; the LD_LIBRARY_PATH variable in projects where I override
    ;; that.
    (let ((process-environment (cl-remove-if
                                (lambda (x) (string-prefix-p "LD_LIBRARY_PATH=" x))
                                process-environment)))
      (apply oldfun args)))
  (map! :map chatgpt-shell-mode-map
        "C-d" #'cae-ai-chatgpt-quit-or-delete-char
        "C-l" #'chatgpt-shell-clear-buffer
        [remap comint-clear-buffer] #'chatgpt-shell-clear-buffer)
  (cae-advice-add #'shell-maker-welcome-message :override #'ignore))

(use-package! forge-llm
  :defer t :after forge :config
  (forge-llm-setup))

(use-package! pi-coding-agent
  :defer t :config
  (after! pi-coding-agent-ui
    (setq pi-coding-agent-input-markdown-highlighting t))
  (when (modulep! :editor evil)
    (after! evil
      (map! (:map pi-coding-agent-input-mode-map
             :n "RET" #'pi-coding-agent-send
             :n "<return>" #'pi-coding-agent-send
             :n "ZQ" #'pi-coding-agent-quit
             :n "<f5>" #'pi-coding-agent-toggle
             :localleader
             "m" #'pi-coding-agent-menu
             "a" #'pi-coding-agent-abort
             "q" #'pi-coding-agent-quit)
            (:map pi-coding-agent-chat-mode-map
             :n "q" #'pi-coding-agent-quit
             :n "ZQ" #'pi-coding-agent-quit
             :n "<f5>" #'pi-coding-agent-toggle
             :localleader
             "m" #'pi-coding-agent-menu
             "a" #'pi-coding-agent-abort
             "q" #'pi-coding-agent-quit)))))

(use-package! fancy-dabbrev
  :when (not (modulep! +fim))
  :defer t :init
  (add-hook 'prog-mode-hook #'fancy-dabbrev-mode)
  (add-hook 'text-mode-hook #'fancy-dabbrev-mode)
  (add-hook 'conf-mode-hook #'fancy-dabbrev-mode)
  :defer t :config
  (setq fancy-dabbrev-preview-context 'before-non-word)
  (map! "C-f" #'cae-fancy-dabbrev-forward-char-or-complete
        "M-f" #'cae-fancy-dabbrev-forward-word-or-complete-word))
