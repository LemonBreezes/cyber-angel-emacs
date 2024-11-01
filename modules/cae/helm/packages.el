;; -*- no-byte-compile: t; -*-
;;; completion/helm/packages.el

(package! cae-lib :recipe (:host github :repo "LemonBreezes/cae-lib"))

(package! helm)

(when (modulep! +childframe)
  (package! helm-posframe))
(when (modulep! +fuzzy)
  (package! helm-flx))
(package! helm-tramp)
