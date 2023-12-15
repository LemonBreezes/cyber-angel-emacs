;; -*- no-byte-compile: t; -*-
;;; private/modeline/packages.el

(package! minions)
(if (modulep! +pretty)
    (package! nyan-mode)
  (package! mlscroll))
(package! anzu)
(when (modulep! :editor evil)
  (package! evil-anzu))
