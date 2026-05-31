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

(defvar cae-packages-bump-review-model)
(setq cae-packages-bump-review-model cae-chat-model)

(after! magit-gptcommit
  (require 'llm-ollama)
  (when (bound-and-true-p cae-ip-address)
    (setq magit-gptcommit-llm-provider
          (make-llm-ollama
           :host cae-ip-address
           :port 11434
           :chat-model cae-coding-agent-model))))

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
  (when (bound-and-true-p cae-ip-address)
    (magit-gptcommit-mode 1)
    (magit-gptcommit-status-buffer-setup)))
(after! git-commit
  (map! :map git-commit-mode-map
        "C-c C-g" #'magit-gptcommit-commit-accept))

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

(use-package! copilot
  :when (and (executable-find "node")
             (modulep! +fim))
  :defer t :init
  (add-hook 'text-mode-hook #'cae-copilot-turn-on-safely)
  (add-hook 'prog-mode-hook #'cae-copilot-turn-on-safely)
  (add-hook 'conf-mode-hook #'cae-copilot-turn-on-safely)
  (cae-advice-add #'copilot--start-agent :around #'cae-shut-up-a)
  (add-hook! 'copilot-disable-predicates
    (defun cae-disable-copilot-in-gptel-p ()
      (bound-and-true-p gptel-mode))
    (defun cae-disable-copilot-in-dunnet-p ()
      (derived-mode-p 'dun-mode))
    (defun cae-multiple-cursors-active-p ()
      (bound-and-true-p multiple-cursors-mode))
    (defun cae-disable-copilot-in-minibuffer ()
      (minibufferp)))
  (setq copilot-install-dir (concat doom-cache-dir "copilot"))
  (autoload 'copilot-clear-overlay "copilot" nil t)
  (cae-defadvice! cae-clear-copilot-overlay-a (&rest _)
    :before #'doom/delete-backward-word
    (copilot-clear-overlay))
  :config
  ;; Assume all Elisp code is formatted with the default indentation style. This
  ;; fixes an error.
  (setf (alist-get 'emacs-lisp-mode copilot-indentation-alist) nil)

  (add-to-list 'copilot-clear-overlay-ignore-commands #'corfu-quit)
  (add-hook! 'doom-escape-hook
    (defun cae-copilot-clear-overlay-h ()
      "Like `copilot-clear-overlay', but returns `t' if the overlay was visible."
      (when (copilot--overlay-visible)
        (copilot-clear-overlay) t)))
  (setq copilot--base-dir
        (expand-file-name ".local/straight/repos/copilot.el/" doom-emacs-dir)
        copilot-max-char 1000000
        copilot-idle-delay 0)
  ;; Model our Copilot interface after Fish completions.
  (map! :map copilot-completion-map
        "<right>" #'copilot-accept-completion
        "C-f" #'copilot-accept-completion
        "M-<right>" #'copilot-accept-completion-by-word
        "M-f" #'copilot-accept-completion-by-word
        "C-e" #'copilot-accept-completion-by-line
        "<end>" #'copilot-accept-completion-by-line
        "M-n" #'copilot-next-completion
        "M-p" #'copilot-previous-completion)
  (remove-hook 'copilot-enable-predicates 'evil-insert-state-p)
  (add-hook! 'copilot-enable-predicates
    (defun cae-evil-insert-state-p ()
      (memq (bound-and-true-p evil-state) '(insert emacs nil))))
  (when (modulep! +copilot)
    (after! copilot
      (add-hook 'yas-before-expand-snippet-hook #'copilot-clear-overlay)))
  (after! copilot-balancer
    (add-to-list 'copilot-balancer-lisp-modes 'fennel-mode)
    (after! midnight
      (add-to-list 'clean-buffer-list-kill-never-buffer-names
                   (buffer-name copilot-balancer-debug-buffer)))))
