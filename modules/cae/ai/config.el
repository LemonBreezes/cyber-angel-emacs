;;; cae/ai/config.el -*- lexical-binding: t; -*-

(require 'cae-lib)

(defvar cae-ai-chatgpt-shell-workspace-name "*chatgpt*")
(defvar cae-ai-dall-e-shell-workspace-name "*dall-e*")

;;; Set the models

;; One model for everything, served natively by Ollama. The Qwen3.8-27B weights
;; ship built-in MTP speculative decoding, so this needs no proxy. Measured on
;; the Q5_K_M quant: 125 tok/s with MTP vs 68 without, a 1.68x speedup.
;;
;; This is the abliterated ("heretic") build of Qwen3.8-27B -- refusal
;; directions removed, with the MTP head and the vision tower left intact
;; upstream. Local tag, built from
;; `llmfan46/Qwen3.8-27B-Ultra-Uncensored-Heretic-Native-MTP-Preserved-GGUF'
;; (Q5_K_M weights plus a separate BF16 mmproj, both needed for vision).
;;
;; Context: the tag sets PARAMETER num_ctx 196608, but Ollama caps usable
;; prompt at num_ctx/2, so the real window is ~98304 tokens. 196608 rather
;; than the model's native 262144 is deliberate -- at 262144 the KV cache
;; overflows 32G VRAM onto host RAM and decode collapses from ~44 to
;; 3.4 tok/s. 196608 holds ~98304 tokens at ~44 tok/s and ~1.8G VRAM spare.
;;
;; Recipe: ~/models/qwen3.8-ablit/Modelfile. Self-contained -- it points at
;; durable, sha256-verified GGUF copies in ~/models/gguf/ rather than at
;; Ollama's blob store, so it still rebuilds after `ollama rm` plus blob
;; collection. Verified by deleting both the model and its blobs, then
;; rebuilding from the Modelfile alone.
;;
;; The stock `qwen3.8:27b-mtp-q4_K_M' (Q4_K_M) is still installed as a
;; censored control for A/B-ing the abliteration.
;;
;; NOTE: cae-coding-fim-model below is currently dead -- nothing in this config
;; reads it. See the FIM notes at the bottom of this file.
(defvar cae-chat-model "qwen3.8:27b-ultra-uncensored-q5_k_m")
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
  ;; (`openai/<model>' + --api-base pointing at ollama's /v1).  The native
  ;; `ollama_chat/' provider in the bundled litellm hangs on streaming (NDJSON
  ;; iterator never yields chunks, even though /api/chat works fine via curl)
  ;; -- the openai/ path streams cleanly through ollama's
  ;; /v1/chat/completions instead.
  ;;
  ;; We pass --api-base / --api-key as explicit aider flags rather than env
  ;; vars: `setenv' in Emacs only propagates to subprocesses spawned by THIS
  ;; Emacs (subtle when aidermacs uses comint), and a stale `OPENAI_API_BASE'
  ;; or no `OPENAI_API_BASE' silently falls back to api.openai.com and fails
  ;; with "Incorrect API key provided: ollama".  Flags can't be ignored.
  (setq cae-aidermacs--api-base   (format "http://%s:11434/v1" cae-ip-address)
        cae-aidermacs--api-key    "ollama")
  ;; This MUST match the model's own num_ctx (196608, set by the derived tag).
  ;; Ollama sizes its KV cache per request, so a mismatch makes the runner
  ;; reallocate every time you switch between aider and pi -- and because
  ;; OLLAMA_MAX_LOADED_MODELS=2 it will then try to hold BOTH allocations at
  ;; once.  At 196608 the model already sits at ~30.4G of 32.6G VRAM once the
  ;; context fills, so a second concurrent runner would OOM.  Matching the tag
  ;; keeps exactly one runner.
  ;;
  ;; Remember Ollama only gives the prompt half of num_ctx, so this env var is
  ;; NOT the usable context: 196608 yields ~98304 real prompt tokens. Aider's
  ;; usable input is capped separately, via max_input_tokens in
  ;; aider-model-metadata.json (81920, the remainder after 16384 output).
  (setenv "OLLAMA_CONTEXT_LENGTH" "196608")
  ;; Use local models for every aider role.  Architect mode pairs the heavy
  ;; reasoner (planner) with the fast in-VRAM coder (applies the edits).  The
  ;; weak model handles cheap chores like commit messages -- the single Qwen3.8
  ;; tag covers every role, so all four slots stay identical.
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
  ;; ollama directly; --model-metadata-file declares real context windows.
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

(use-package! pilish
  :defer t :config
  (when (modulep! :editor evil)
    (after! evil
      (map! (:map pilish-input-mode-map
             :g "<f5>" #'pilish-toggle
             :localleader
             "m" #'pilish-menu
             "a" #'pilish-abort
             "q" #'pilish-quit)
            (:map pilish-chat-mode-map
             :n "q" #'pilish-quit
             :n "ZQ" #'pilish-quit
             :n "<f5>" #'pilish-toggle
             :localleader
             "m" #'pilish-menu
             "a" #'pilish-abort
             "q" #'pilish-quit)))))

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
             (modulep! +copilot))
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
