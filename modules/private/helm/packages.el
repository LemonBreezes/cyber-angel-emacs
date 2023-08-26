;; -*- no-byte-compile: t; -*-
;;; completion/helm/packages.el

(package! helm)

(when (modulep! +childframe)
  (package! helm-posframe))
(when (modulep! +fuzzy)
  (package! helm-flx))
(package! helm-tramp)
