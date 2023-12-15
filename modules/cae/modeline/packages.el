;; -*- no-byte-compile: t; -*-
;;; private/modeline/packages.el

(package! minions)
(package! nyan-mode)
(package! anzu)
(when (modulep! :editor evil)
  (package! evil-anzu))
