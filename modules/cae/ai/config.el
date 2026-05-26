;;; cae/ai/config.el -*- lexical-binding: t; -*-

(require 'cae-lib)

(defvar cae-ai-chatgpt-shell-workspace-name "*chatgpt*")
(defvar cae-ai-dall-e-shell-workspace-name "*dall-e*")

(use-package! aidermacs
  :defer t :init
  (autoload 'aidermacs-transient-menu "aidermacs" nil t)
  :config
  (setq aidermacs-default-model "openrouter/google/gemini-3.1-pro-preview")
  (setq aidermacs-editor-model "anthropic/claude-opus-4-7")
  (setq aidermacs-auto-commits nil)
  (setq aidermacs-backend 'comint)
  (setq aidermacs-extra-args
        '("--cache-prompts"
          "--cache-keepalive-pings" "6"
          "--watch-files"
          ;;"--auto-test"
          ;;"--timeout" "120"
          ;;"--test"
          ;;"--auto-commits"
          "--auto-accept-architect"
          ;;"--install-tree-sitter-language-pack"
          "--chat-language" "English"
          ;;"--editor-edit-format" "editor-whole"
          ))
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
  (setq magit-gptcommit-prompt
        "You are an expert programmer writing a Git commit message.
You have carefully reviewed every file diff included in this commit.

First, choose the most appropriate label for the changes.
Here are the labels you can choose from:
- build: Changes that affect the build system or external dependencies (e.g., gulp, broccoli, npm)
- chore: Routine tasks like updating dependencies, licenses, or repo settings
- ci: Changes to CI configuration files or scripts (e.g., GitHub Actions, CircleCI)
- docs: Documentation-only changes (e.g., fixing typos, adding examples)
- feat: Introduces a new feature to the codebase
- fix: Patches a bug in the codebase
- perf: Improves performance without changing behavior
- refactor: Code changes that neither fix bugs nor add features
- style: Non-functional changes like formatting or whitespace
- test: Adds or corrects tests

Next, write a high-level summary of the commit.
- Keep it to a single line, no more than 50 characters
- Use the imperative tense (e.g., 'Add logging' not 'Added logging')
- Ensure the message reflects a clear and cohesive change
- Do not end the summary with a period
- Do not use backticks (`) anywhere in the response
- **Do not use square brackets ([]) around the label or the summary**

THE FILE DIFFS:
```
%s
```

Now, write the commit message using the Conventional Commits format: **label: summary** (e.g., fix: resolve memory leak)")
  (setq magit-gptcommit-prompt-one-line "You are an expert programmer writing a Git commit message.
You have carefully reviewed every file diff included in this commit.

First, choose the most appropriate label for the changes.
Here are the labels you can choose from:
- build: Changes that affect the build system or external dependencies (e.g., gulp, broccoli, npm)
- chore: Routine tasks like updating dependencies, licenses, or repo settings
- ci: Changes to CI configuration files or scripts (e.g., GitHub Actions, CircleCI)
- docs: Documentation-only changes (e.g., fixing typos, adding examples)
- feat: Introduces a new feature to the codebase
- fix: Patches a bug in the codebase
- perf: Improves performance without changing behavior
- refactor: Code changes that neither fix bugs nor add features
- style: Non-functional changes like formatting or whitespace
- test: Adds or corrects tests

Next, write a high-level summary of the commit.
- Keep it to a single line, no more than 50 characters
- Use the imperative tense (e.g., 'Add logging' not 'Added logging')
- Ensure the message reflects a clear and cohesive change
- Do not end the summary with a period
- Do not use backticks (`) anywhere in the response
- Do not use square brackets ([]) around the label or the summary

THE FILE DIFFS:
```
%s
```
Now, write the commit message using the Conventional Commits format: label: summary (e.g., fix: resolve memory leak)")
  (magit-gptcommit-mode 1)
  (magit-gptcommit-status-buffer-setup))
(after! git-commit
  (map! :map git-commit-mode-map
        "C-c C-g" #'magit-gptcommit-commit-accept))

;; Not really a fan of this package yet.
(use-package! minuet
  :when (modulep! +fim)
  :defer t :init
  ;; With my GPU, Minuet is too slow for auto-suggestions becaus it has to load
  ;; the model into VRAM.
  ;;(add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
  ;;(add-hook 'text-mode-hook #'minuet-auto-suggestion-mode)
  ;;(add-hook 'conf-mode-hook #'minuet-auto-suggestion-mode)
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

(when (modulep! +local)
  (load! "+local"))
