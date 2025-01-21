;;; cae/ai/config.el -*- lexical-binding: t; -*-

(require 'cae-lib)

(defvar cae-ai-chatgpt-shell-workspace-name "*chatgpt*")
(defvar cae-ai-dall-e-shell-workspace-name "*dall-e*")

;; Set up the default models.
(let ((claude-model "claude-3-5-sonnet-20240620"))
  (after! chatgpt-shell
    (setq chatgpt-shell-model-version 1
          chatgpt-shell-streaming t))
  (advice-add #'chatgpt-shell-system-prompt :override #'ignore)
  (after! gptel
    (setq gptel-model claude-model
          gptel-backend (gptel-make-anthropic "Claude"
                          :stream t :key (cae-secrets-get-anthropic-api-key))))
  (after! dall-e-shell
    (setq dall-e-shell-model-version "dall-e-3"))
  (after! aider
    (setq aider-args
          `(
            "--o1-preview"
            "--editor-model" "o1-preview"
            "--openai-api-key" ,(cae-secrets-get-openai-api-key)
            "--anthropic-api-key" ,(cae-secrets-get-anthropic-api-key)
            ;;"--encoding" "latin-1"
            "--cache-prompts"
            "--chat-language" "English"
            )))
  (defvar llm-refactoring-provider nil)
  (after! llm
    (require 'llm-claude)
    (setq llm-refactoring-provider
          (make-llm-claude :chat-model claude-model
                           :key (cae-secrets-get-anthropic-api-key)
                           ;;:default-chat-non-standard-params '((stream . :json-false))
                           )
          magit-gptcommit-llm-provider llm-refactoring-provider
          llm-warn-on-nonfree nil)))

(use-package! aider
  :defer t :init
  (defalias 'aider-read-string #'aider-plain-read-string))

(use-package! magit-gptcommit
  :after gptel magit
  :init
  (advice-add 'magit-gptcommit-commit-accept :after
              (lambda ()
                (when-let ((buf (magit-commit-message-buffer)))
                  (with-current-buffer buf
                    (save-buffer)))))
  :config
  ;; Enable magit-gptcommit-mode to watch staged changes and generate commit message automatically in magit status buffer
  ;; This mode is optional, you can also use `magit-gptcommit-generate' to generate commit message manually
  ;; `magit-gptcommit-generate' should only execute on magit status buffer currently
  (magit-gptcommit-mode 1)
  ;; Add gptcommit transient commands to `magit-commit'
  ;; Eval (transient-remove-suffix 'magit-commit '(1 -1)) to remove gptcommit transient commands
  (magit-gptcommit-status-buffer-setup))
(after! git-commit
  (map! :map git-commit-mode-map
        "C-c C-g" #'magit-gptcommit-commit-accept))

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
      (bound-and-true-p multiple-cursors-mode))
    (defun cae-disable-copilot-in-minibuffer ()
      (minibufferp)))
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
  (add-hook 'yas-before-expand-snippet-hook #'copilot-clear-overlay)
  (after! copilot-balancer
    (add-to-list 'copilot-balancer-lisp-modes 'fennel-mode)
    (after! midnight
      (add-to-list 'clean-buffer-list-kill-never-buffer-names
                   (buffer-name copilot-balancer-debug-buffer)))))

(use-package! dall-e-shell
  :defer t :init
  ;;(map! :leader
  ;;      :prefix "o"
  ;;      :desc "Open DALL-E here" "I" #'dall-e-shell
  ;;      :desc "Toggle DALL-E popup" "i" #'cae-ai-toggle-dall-e-shell
  ;;      :desc "Open DALL-E workspace" "C-i" #'cae-ai-open-dall-e-workspace)
  :config
  (setq dall-e-shell-display-function #'switch-to-buffer
        dall-e-shell-openai-key (cae-secrets-get-openai-api-key)
        dall-e-shell-image-quality "hd"
        dall-e-shell-image-size "1024x1792"
        dall-e-shell-request-timeout 180))
(use-package! chatgpt-shell
  :defer t :init
  (map! :leader
        :prefix "o"
        :desc "Toggle ChatGPT popup" "c" #'cae-ai-toggle-chatgpt-shell
        :desc "Open ChatGPT here" "C" #'chatgpt-shell
        :desc "Open ChatGPT workspace" "C-c" #'cae-ai-open-chatgpt-workspace)
  ;; Use , to ask ChatGPT questions in any comint buffer
  ;;(advice-add 'comint-send-input :around 'cae-send-to-chatgpt-if-comma-a)
  :config
  (unless (executable-find "dwdiff")
    (warn "dwdiff is not installed, so ChatGPT shell will not be able to use it."))
  (setq chatgpt-shell-display-function #'switch-to-buffer)
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

(use-package! gptel
  :defer t :config
  (after! evil
    (evil-set-initial-state #'gptel-context-buffer-mode 'emacs)))
