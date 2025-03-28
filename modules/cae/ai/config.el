;;; cae/ai/config.el -*- lexical-binding: t; -*-

(require 'cae-lib)

(defvar cae-ai-chatgpt-shell-workspace-name "*chatgpt*")
(defvar cae-ai-dall-e-shell-workspace-name "*dall-e*")

;; Set up the default models.
(let ((claude-model "claude-3-7-sonnet-20250219")
      (gemini-model "gemini-2.5-pro-exp-03-25"))
  ;; ChatGPT Shell, Dall-E Shell, LLM, GPTel, and Minuet.
  ;; Aider.
  (after! chatgpt-shell
    (cl-pushnew
     (chatgpt-shell-anthropic--make-model
      :version claude-model
      :short-version (string-remove-prefix "claude-" claude-model)
      :token-width  4
      :max-tokens 8192
      :context-window 200000)
     chatgpt-shell-models)
    (setq chatgpt-shell-model-version claude-model))
  (after! dall-e-shell
    (setq dall-e-shell-model-version "dall-e-3"))
  (defvar llm-refactoring-provider nil)
  (after! llm
    (require 'llm-claude)
    (setq llm-refactoring-provider
          (make-llm-claude :chat-model claude-model
                           :key (getenv "ANTHROPIC_API_KEY")
                           ;;:default-chat-non-standard-params '((stream . :json-false))
                           )
          magit-gptcommit-llm-provider llm-refactoring-provider
          llm-warn-on-nonfree nil))
  (after! gptel
    (setq gptel-backend
          (gptel-make-anthropic "Claude-thinking" ;Any name you want
            :key (getenv "ANTHROPIC_API_KEY")
            :stream t
            :models `(,(intern claude-model))
            :header (lambda () (when-let* ((key (gptel--get-api-key)))
                                 `(("x-api-key" . ,key)
                                   ("anthropic-version" . "2023-06-01")
                                   ("anthropic-beta" . "pdfs-2024-09-25")
                                   ("anthropic-beta" . "output-128k-2025-02-19")
                                   ("anthropic-beta" . "prompt-caching-2024-07-31"))))
            :request-params '(:thinking (:type "enabled" :budget_tokens 2048)
                              :max_tokens 4096)))
    (gptel-make-deepseek "DeepSeek"
      :stream t
      :key (getenv "DEEPSEEK_API_KEY"))
    (setq gptel-model (intern claude-model)))
  (after! minuet
    (setq minuet-provider 'claude)
    (plist-put! minuet-claude-options
                :model "sonnet"
                :max_tokens 256
                :top_p 0.9)
    (plist-put! minuet-codestral-options
                :max_tokens 256
                :top_p 0.9)))

(use-package! aidermacs
  :defer t :init
  (autoload 'aidermacs-transient-menu "aidermacs" nil t)
  :config
  ;;(setq aidermacs-default-model "anthropic/claude-3-7-sonnet-20250219")
  (setq aidermacs-default-model "gemini/gemini-2.5-pro-exp-03-25")
  (setq aidermacs-editor-model "gemini/gemini-2.5-pro-exp-03-25")
  (setq aidermacs-editor-model "anthropic/claude-3-5-haiku-latest")
  (setq aidermacs-auto-commits nil)
  (setq aidermacs-use-architect-mode t)
  (setq aidermacs-auto-accept-architect t)
  (setq aidermacs-backend 'comint)
  (setq aidermacs-extra-args
        '("--cache-prompts"
          "--cache-keepalive-pings" "6"
          "--watch-files"
          ;;"--auto-test"
          ;;"--timeout" "120"
          ;;"--test"
          "--auto-commits"
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
  :after magit :config
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
  (when (modulep! ai +copilot)
    (after! copilot
      (add-hook 'yas-before-expand-snippet-hook #'copilot-clear-overlay)))
  (after! copilot-balancer
    (add-to-list 'copilot-balancer-lisp-modes 'fennel-mode)
    (after! midnight
      (add-to-list 'clean-buffer-list-kill-never-buffer-names
                   (buffer-name copilot-balancer-debug-buffer)))))

(use-package! minuet
  :when (modulep! -copilot)
  :defer t :init
  (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
  (add-hook 'text-mode-hook #'minuet-auto-suggestion-mode)
  (add-hook 'conf-mode-hook #'minuet-auto-suggestion-mode)
  :config
  (map! :map minuet-active-mode-map
        :ig "M-p" #'minuet-previous-suggestion ;; invoke completion or cycle to next completion
        :ig "M-n" #'minuet-next-suggestion ;; invoke completion or cycle to previous completion
        :ig "C-f" #'minuet-accept-suggestion ;; accept whole completion
        :ig "C-e" #'minuet-accept-suggestion-line)
  (add-hook! 'doom-escape-hook :depth -1
    (defun cae-minuet-dismiss-suggestion-h ()
      (when minuet--current-overlay
        (minuet-dismiss-suggestion)
        t)))
  (add-hook 'evil-insert-state-exit-hook #'minuet-dismiss-suggestion)
  (when (modulep! :editor evil)
    (add-hook 'minuet-active-mode-hook #'evil-normalize-keymaps)))

(use-package! dall-e-shell
  :defer t :init
  ;;(map! :leader
  ;;      :prefix "o"
  ;;      :desc "Open DALL-E here" "I" #'dall-e-shell
  ;;      :desc "Toggle DALL-E popup" "i" #'cae-ai-toggle-dall-e-shell
  ;;      :desc "Open DALL-E workspace" "C-i" #'cae-ai-open-dall-e-workspace)
  :config
  (setq dall-e-shell-display-function #'switch-to-buffer
        dall-e-shell-openai-key (getenv "OPENAI_API_KEY")
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
  (cae-advice-add 'comint-send-input :around 'cae-send-to-chatgpt-if-comma-a)
  ;; Add eshell support for comma prefix
  (add-hook 'eshell-input-filter-functions #'cae-eshell-send-to-chatgpt-if-comma)
  :config
  (map! :map chatgpt-shell-mode-map
        :n "RET" #'comint-send-input
        :i "RET" #'+default/newline)
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

(use-package! gptel
  :defer t :init
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (autoload 'gptel--stream-convert-markdown->org "gptel-org")
  :config
  (setq gpt-openai-key (getenv "OPENAI_API_KEY"))
  (setq gptel-default-mode 'org-mode)
  (after! gptel-context
    (map! :map gptel-context-buffer-mode-map
          :n "q" #'gptel-context-quit
          :n "n" #'gptel-context-next
          :n "p" #'gptel-context-previous
          :n "d" #'gptel-context-flag-deletion
          :n "RET" #'gptel-context-visit
          :map gptel-mode-map
          :n "<return>" #'gptel-send))
  (add-hook! 'gptel-mode-hook
    (defun cae-gptel-mode-setup-h ()
      (setq-local nobreak-char-display nil)
      (auto-fill-mode -1)
      (doom-mark-buffer-as-real-h)))
  ;; BUG Karthink refused to accept my PR to fix this, saying that starting the
  ;; point on top of the "Rewrite: " is a good idea. I disagree.
  (cae-defadvice! cae-goto-point-max-a (_)
    :after #'gptel--read-with-prefix
    (goto-char (point-max))))

(use-package! forge-llm
  :defer t :after forge :config
  (forge-llm-setup))
