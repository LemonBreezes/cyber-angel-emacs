;;; private/ai/+whisper.el -*- lexical-binding: t; -*-

(after! org-ai
  (require 'whisper)
  (require 'greader-espeak)
  (require 'greader)
  (setq org-ai-talk-say-words-per-minute 210)
  (setq org-ai-talk-say-voice "Karen"))

;; example usage:
;; (rk/find-device-matching "FaceTime" :video)
;; (rk/find-device-matching "Macbook Pro Microphone" :audio)
;; (rk/select-default-audio-device)

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
