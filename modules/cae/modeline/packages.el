;; -*- no-byte-compile: t; -*-
;;; private/modeline/packages.el

(package! minions)
(when (modulep! +pretty)
  (package! nyan-mode))
(package! anzu)
(when (modulep! :editor evil)
  (package! evil-anzu))
(package! mini-echo :recipe (:host github :repo "liuyinz/mini-echo.el"))
