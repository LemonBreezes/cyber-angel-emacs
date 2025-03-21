;; -*- no-byte-compile: t; -*-
;;; cae/ai/packages.el

(package! cae-lib :recipe (:host github :repo "LemonBreezes/cae-lib"))

(package! gptel)
(package! aidermacs :recipe (:host github :repo "MatthewZMD/aidermacs" :files ("*.el")))
(package! magit-gptcommit :recipe (:host github :repo "LemonBreezes/magit-gptcommit"
                                   :branch "fix-uncaught-plz-event-error"))
(package! chatgpt-shell)
(package! dall-e-shell)
(when (modulep! +copilot)
  (package! copilot :recipe (:host github :repo "copilot-emacs/copilot.el")))
(when (modulep! -copilot)
  (package! minuet))
(package! forge-llm
  :recipe (:host gitlab :repo "rogs/forge-llm"))
