;; -*- no-byte-compile: t; -*-
;;; private/helm/packages.el

(when (modulep! :app emms)
  (package! helm-emms))
(package! helm-descbinds)
(when (modulep! +icons)
  (package! helm-icons))
(when (modulep! +fuzzy)
  (package! helm-flx))
