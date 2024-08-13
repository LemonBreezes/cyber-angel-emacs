;; -*- no-byte-compile: t; -*-
;;; private/ai/packages.el

(package! org-ai)
(package! greader)
(package! whisper :recipe (:host github :repo "natrys/whisper.el"))
(package! chatgpt-shell)
(package! dall-e-shell)
(package! copilot
  :recipe (:host github :repo "LemonBreezes/copilot.el" :branch "fix-copilot-server-executable-var" :files ("*.el" "dist")))
(package! gptel)
(package! magit-gptcommit)
