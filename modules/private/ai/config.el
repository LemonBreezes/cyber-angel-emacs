;;; private/ai/config.el -*- lexical-binding: t; -*-

(use-package! org-ai
  :defer t :init
  (map! :desc "+org-ai-prefix" "C-c M-a" #'cae-ai-lazy-load-org-ai)
  (autoload 'org-ai-mode "org-ai" nil t)
  (add-hook 'org-mode-hook #'org-ai-mode)
  (define-prefix-command 'org-ai-region-map)
  (map! :map org-ai-region-map
        "s" #'org-ai-summarize
        "r" #'org-ai-on-region
        "R" #'org-ai-talk-read-region
        "c" #'org-ai-refactor-code)
  (after! embark
    (map! :map embark-region-map
          "M-a" #'org-ai-region-map))
  :config
  (org-ai-global-mode +1)
  (map! :map org-ai-global-mode-map
        :prefix ("C-c M-a" . "org-ai")
        "b" #'cae-ai-org-ai-on-buffer)
  (map! :map org-ai-mode-map
        [remap org-ai-kill-region-at-point] #'cae-ai-org-ai-kill-region-at-point)
  (defvar org-ai-global-mode-prefix-map
    (lookup-key org-ai-global-mode-map (kbd "C-c M-a")))
  (setq org-ai-default-chat-model "gpt-4"
        org-ai-on-project-modify-with-diffs t)
  (when (modulep! :editor snippets)
    (org-ai-install-yasnippets)))

(use-package! chatgpt-shell
  :defer t :init
  (setq chatgpt-shell-display-function #'switch-to-buffer)
  (map! :leader
        :prefix "o"
        :desc "Toggle ChatGPT popup" "c" #'cae-ai-toggle-chatgpt-shell
        :desc "Open ChatGPT here" "C" #'chatgpt-shell)
  :config
  (advice-add #'shell-maker-async-shell-command
              :around
              (cae-defun cae-ai-ignore-ld-library-path-a (oldfun &rest args)
                ;; This is a hack to prevent the ChatGPT shell from inheriting
                ;; the LD_LIBRARY_PATH variable in projects where I override
                ;; that.
                (let ((process-environment (cl-remove-if
                                            (lambda (x) (string-prefix-p "LD_LIBRARY_PATH=" x))
                                            process-environment)))
                  (apply oldfun args))))
  (define-key chatgpt-shell-mode-map (kbd "C-d") #'cae-ai-chatgpt-quit-or-delete-char)
  (define-key chatgpt-shell-mode-map (kbd "<f6>") #'cae-chatgpt-shell-cheatsheet-hydra/body)
  (advice-add #'shell-maker-welcome-message :override #'ignore))

(use-package! gpt-commit
  :defer t :init
  (add-hook 'git-commit-setup-hook 'gpt-commit-message)
  :config
  (setq gpt-commit-model "gpt-4"))
