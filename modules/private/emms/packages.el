;; -*- no-byte-compile: t; -*-
;;; private/emms/packages.el

(package! emms)
(when (or (modulep! :private helm)
          (modulep! :completion helm))
  (package! helm-emms))
