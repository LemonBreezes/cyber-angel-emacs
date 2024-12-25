;;; cae/ai/trash/config.el -*- lexical-binding: t; -*-

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

(when (modulep! +ollama)
  (load! "+ollama"))

(use-package! elysium
  :defer t :autoload (elysium-query)
  :custom
  (elysium-window-size 0.5)
  (elysium-window-style 'vertical))

(use-package! gptel
  :defer t :init
  :config
  (setq gptel-default-mode 'org-mode
        ;; Fixes some malformed JSON response error.
        gptel-use-curl nil))
