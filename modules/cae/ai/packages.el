;; -*- no-byte-compile: t; -*-
;;; cae/ai/packages.el

(package! cae-lib :recipe (:host github :repo "LemonBreezes/cae-lib"))

(package! gptel :recipe (:nonrecursive t))
(package! aidermacs :recipe (:host github :repo "MatthewZMD/aidermacs" :files ("*.el")))
(package! chatgpt-shell)
(package! dall-e-shell)
(when (modulep! +copilot)
  (package! copilot :recipe (:host github :repo "copilot-emacs/copilot.el")))
(when (modulep! -copilot)
  (package! minuet))
(package! forge-llm
  :recipe (:host gitlab :repo "rogs/forge-llm"))

;; `magit-gptcommit' is much better.
(package! magit-gptcommit :recipe (:host github :repo "douo/magit-gptcommit"))
(disable-packages! gptel-magit)
