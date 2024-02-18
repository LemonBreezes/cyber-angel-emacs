;;; private/ai/config.el -*- lexical-binding: t; -*-

(defvar cae-openai-default-model "gpt-4-1106-preview")

(use-package! whisper
  :defer t :init
  (map! "<f10>" #'whisper-run)
  :config
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
  (map! :nv "g!" #'cae-org-ai-on-region-or-buffer
        "<f5>" #'cae-org-ai-on-region-or-buffer)
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
  (if (member cae-openai-default-model chatgpt-shell-model-versions)
      (setq chatgpt-shell-model-version
            (seq-position chatgpt-shell-model-versions cae-openai-default-model))
    (setq chatgpt-shell-model-versions
          (cons cae-openai-default-model chatgpt-shell-model-versions)
          chatgpt-shell-model-version 0))
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
  (define-key chatgpt-shell-mode-map (kbd "C-d") #'cae-ai-chatgpt-quit-or-delete-char)
  (map! :map chatgpt-shell-mode-map
        [remap comint-clear-buffer] #'chatgpt-shell-clear-buffer)
  (advice-add #'shell-maker-welcome-message :override #'ignore))

(use-package! copilot
  :defer t :init
  (add-hook 'text-mode-hook   #'copilot-mode)
  (add-hook 'prog-mode-hook   #'copilot-mode)
  (add-hook 'conf-mode-hook   #'copilot-mode)
  (advice-add #'copilot--start-agent :around #'cae-shut-up-a)
  :config
  (setq copilot--base-dir
        (expand-file-name ".local/straight/repos/copilot.el/" doom-emacs-dir)
        copilot-max-char 1000000)
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
  (defun cae-copilot-clear-overlay-h ()
    "Like `copilot-clear-overlay', but returns `t' if the overlay was visible."
    (when (copilot--overlay-visible)
      (copilot-clear-overlay) t))
  (add-hook 'doom-escape-hook #'cae-copilot-clear-overlay-h)
  (add-hook! 'copilot-disable-predicates
    (defun cae-disable-copilot-in-gptel-p ()
      (bound-and-true-p gptel-mode))
    (defun cae-disable-copilot-in-dunnet-p ()
      (bound-and-true-p dun-mode))
    (defun cae-multiple-cursors-active-p ()
      (bound-and-true-p multiple-cursors-mode)))
  (add-hook 'yas-before-expand-snippet-hook #'copilot-clear-overlay)
  (after! copilot-balancer
    (add-to-list 'copilot-balancer-lisp-modes 'fennel-mode)
    (after! midnight
      (add-to-list 'clean-buffer-list-kill-never-buffer-names
                   (buffer-name copilot-balancer-debug-buffer)))))
