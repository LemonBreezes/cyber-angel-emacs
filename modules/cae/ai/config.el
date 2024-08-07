;;; private/ai/config.el -*- lexical-binding: t; -*-


(defvar cae-openai-default-model "gpt-4o")

(use-package! whisper
  :defer t :config
  (setq whisper-install-directory doom-cache-dir
        whisper-model "large-v3"
        whisper-language "en"
        whisper-translate nil
        whisper-use-threads (num-processors)))

(use-package! org-ai
  :commands (org-ai-on-region)
  :defer t :init
  (map! :desc "org-ai-prefix" "C-c M-a"
        (cae-oneshot-keymap org-ai-global-mode-prefix-map org-ai))
  (autoload 'org-ai-mode "org-ai" nil t)
  (add-hook 'org-mode-hook #'org-ai-mode)
  :config
  (require 'whisper)
  (require 'greader-espeak)
  (require 'greader)
  (setq org-ai-talk-say-words-per-minute 210
        org-ai-talk-say-voice "Karen"
        org-ai-on-project-max-files 300)
  (org-ai-global-mode +1)
  (map! :map org-ai-mode-map
        [remap org-ai-kill-region-at-point] #'cae-ai-org-ai-kill-region-at-point
        :map org-ai-on-project-mode-map
        ;; Do not accidentally quit `org-ai-on-project-mode' when trying to type `q'.
        :i "q" #'self-insert-command)
  (defvar org-ai-global-mode-prefix-map
    (lookup-key org-ai-global-mode-map (kbd "C-c M-a")))
  (setq org-ai-default-chat-model cae-openai-default-model
        org-ai-on-project-modify-with-diffs t)
  (when (modulep! :editor snippets)
    (org-ai-install-yasnippets)))

(use-package! gptel
  :defer t :config
  (setq gptel-model cae-openai-default-model))

;; I need to configure an `llm' provider.
(use-package! magit-gptcommit
  :after gptel magit
  :config
  ;; Enable magit-gptcommit-mode to watch staged changes and generate commit message automatically in magit status buffer
  ;; This mode is optional, you can also use `magit-gptcommit-generate' to generate commit message manually
  ;; `magit-gptcommit-generate' should only execute on magit status buffer currently
  (magit-gptcommit-mode 1)
  ;; Add gptcommit transient commands to `magit-commit'
  ;; Eval (transient-remove-suffix 'magit-commit '(1 -1)) to remove gptcommit transient commands
  (magit-gptcommit-status-buffer-setup)
  :bind (:map git-commit-mode-map
         ("C-c C-g" . magit-gptcommit-commit-accept)))

(use-package! dall-e-shell
  :defer t :init
  (map! :leader
        :prefix "o"
        :desc "Open DALL-E here" "I" #'dall-e-shell)
  :config
  (setq dall-e-shell-display-function #'switch-to-buffer
        dall-e-shell-openai-key openai-api-key
        dall-e-shell-image-quality "hd"
        dall-e-shell-image-size "1024x1792"
        dall-e-shell-request-timeout 180
        dall-e-shell-model-version "dall-e-3"))
(use-package! chatgpt-shell
  :defer t :init
  (map! :leader
        :prefix "o"
        :desc "Toggle ChatGPT popup" "c" #'cae-ai-toggle-chatgpt-shell
        :desc "Open ChatGPT here" "C" #'chatgpt-shell)
  ;; Use , to ask ChatGPT questions in any comint buffer
  ;;(advice-add 'comint-send-input :around 'cae-send-to-chatgpt-if-comma-a)
  :config
  (setq chatgpt-shell-display-function #'switch-to-buffer
        chatgpt-shell-model-version 2)
  (when (modulep! +openai)
    (if (member cae-openai-default-model chatgpt-shell-model-versions)
        (setq chatgpt-shell-model-version
              (seq-position chatgpt-shell-model-versions cae-openai-default-model))
      (setq chatgpt-shell-model-versions
            (cons cae-openai-default-model chatgpt-shell-model-versions)
            chatgpt-shell-model-version 0)))
  ;; Trying to stop some escape codes from showing up in my ChatGPT shell.
  (setq-hook! 'chatgpt-shell-mode-hook
    comint-process-echoes t)
  (defadvice! cae-ai-ignore-ld-library-path-a (oldfun &rest args)
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
  (advice-add #'shell-maker-welcome-message :override #'ignore))

(use-package! copilot
  :defer t :init
  (add-hook 'text-mode-hook   #'copilot-mode)
  (add-hook 'prog-mode-hook   #'copilot-mode)
  (add-hook 'conf-mode-hook   #'copilot-mode)
  (advice-add #'copilot--start-agent :around #'cae-shut-up-a)
  (add-hook! 'copilot-disable-predicates
    (defun cae-disable-copilot-in-gptel-p ()
      (bound-and-true-p gptel-mode))
    (defun cae-disable-copilot-in-dunnet-p ()
      (derived-mode-p 'dun-mode))
    (defun cae-multiple-cursors-active-p ()
      (bound-and-true-p multiple-cursors-mode)))
  :config
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
  (add-hook 'yas-before-expand-snippet-hook #'copilot-clear-overlay)
  (after! copilot-balancer
    (add-to-list 'copilot-balancer-lisp-modes 'fennel-mode)
    (after! midnight
      (add-to-list 'clean-buffer-list-kill-never-buffer-names
                   (buffer-name copilot-balancer-debug-buffer)))))

(defvar llm-refactoring-provider nil)
(after! llm
  (require 'llm-openai)
  (setq llm-refactoring-provider (make-llm-openai :key openai-api-key
                                                  :chat-model cae-openai-default-model)
        magit-gptcommit-llm-provider llm-refactoring-provider
        llm-warn-on-nonfree nil))

(when (modulep! +ollama)
  (load! "+ollama"))
