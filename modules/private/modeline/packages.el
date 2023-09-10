;; -*- no-byte-compile: t; -*-
;;; private/modeline/packages.el

(package! minions)
(when (modulep! +pretty)
  (package! nyan-mode))
(package! parrot :recipe (:host github :repo "positron-solutions/parrot"))
