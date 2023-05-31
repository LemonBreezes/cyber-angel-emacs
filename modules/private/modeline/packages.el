;; -*- no-byte-compile: t; -*-
;;; private/modeline/packages.el

(package! minions :recipe (:host github :repo "LemonBreezes/minions"))
(when (modulep! +pretty)
  (package! nyan-mode)
  (package! parrot :recipe (:host github :repo "LemonBreezes/parrot-1")))
