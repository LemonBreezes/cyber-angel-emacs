;; -*- no-byte-compile: t; -*-
;;; completion/helm/packages.el

(package! helm)

(when (modulep! +childframe)
  (package! helm-posframe :recipe (:host github :repo "LemonBreezes/helm-posframe" :branch "fix-window-dedicated-p-error")))
(when (modulep! +fuzzy)
  (package! helm-flx))
