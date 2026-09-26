;;; cae/ai/config.el -*- lexical-binding: t; -*-

(require 'cae-lib)

(defvar cae-ai-chatgpt-shell-workspace-name "*chatgpt*")
(defvar cae-ai-dall-e-shell-workspace-name "*dall-e*")

;;; Set the models

;; One model for everything, served natively by Ollama. Qwen3.8-27B ships
;; built-in MTP speculative decoding, so this needs no proxy: ~186 tok/s,
;; 2.7x the same weights without draft (measured, 90-96% draft acceptance).
;;
;; The `-262k' suffix is a LOCAL derived tag, not an upstream one. The official
;; `qwen3.8:27b-mtp-q4_K_M' inherits the server's OLLAMA_CONTEXT_LENGTH=65536,
;; so qwen3.8-mtp-262k.Modelfile re-issues it with PARAMETER num_ctx 262144
;; (the model's native window; confirmed by needle-retrieval at 175k tokens).
;; Recipe: ~/.config/ollama/qwen3.8-mtp-262k.Modelfile
;; Sibling tags -128k and bare (64k) exist as VRAM fallbacks; all three share
;; weight blobs, so the whole set costs 17.7G total.
;;
;; NOTE: cae-coding-fim-model below is currently dead -- nothing in this config
;; reads it. See the FIM notes at the bottom of this file.
(defvar cae-chat-model "qwen3.8:27b-mtp-q4_K_M-262k")
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
  ;; This MUST match the model's own num_ctx (262144, set by the derived tag).
  ;; Ollama sizes its KV cache per request, so a mismatch makes the runner
  ;; reallocate every time you switch between aider and pi -- and because
  ;; OLLAMA_MAX_LOADED_MODELS=2 it will then try to hold BOTH allocations at
  ;; once.  At 262144 the model already sits at ~30G of 32G VRAM, so a second
  ;; concurrent runner would OOM.  Matching the tag keeps exactly one runner.
  ;; Aider's usable input is capped separately, via max_input_tokens in
  ;; aider-model-metadata.json.
  (setenv "OLLAMA_CONTEXT_LENGTH" "262144")
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
