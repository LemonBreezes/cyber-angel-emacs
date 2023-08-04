;;; private/ai/+whisper.el -*- lexical-binding: t; -*-

(after! org-ai
  (require 'whisper)
  (require 'greader-espeak)
  (require 'greader)
  (setq org-ai-talk-say-words-per-minute 210)
  (setq org-ai-talk-say-voice "Karen"))

(use-package! whisper
  :defer t
  :bind ("<f12>" . whisper-run)
  :config
  (setq whisper-install-directory doom-cache-dir
        whisper-model "base"
        whisper-language "en"
        whisper-translate nil
        whisper-use-threads (num-processors)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(provide 'init-org-ai)
