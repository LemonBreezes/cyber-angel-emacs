;; -*- no-byte-compile: t; -*-
;;; private/modeline/packages.el

(package! minions)
(when (modulep! +pretty)
  (package! nyan-mode))
(package! anzu)
(when (modulep! :editor evil)
  (package! evil-anzu))
(package! awesome-tray :recipe (:host github :repo "manateelazycat/awesome-tray"))
