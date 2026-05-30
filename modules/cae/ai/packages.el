;; -*- no-byte-compile: t; -*-
;;; cae/ai/packages.el

(package! cae-lib :recipe (:host github :repo "LemonBreezes/cae-lib"))

(package! aidermacs :recipe (:host github :repo "MatthewZMD/aidermacs" :files ("*.el")))
(when (modulep! +fim)
  (package! copilot :recipe (:host github :repo "copilot-emacs/copilot.el")))
(package! forge-llm
  :recipe (:host gitlab :repo "rogs/forge-llm"))
(package! pi-coding-agent)
(package! fancy-dabbrev)

;; `magit-gptcommit' is much better.
(package! magit-gptcommit)
(disable-packages! gptel-magit)
