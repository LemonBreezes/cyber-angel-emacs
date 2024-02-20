;; -*- no-byte-compile: t; -*-
;;; private/modeline/packages.el

(package! minions)
(when (modulep! +pretty)
  (package! nyan-mode))
(package! anzu)
(when (modulep! :editor evil)
  (package! evil-anzu))
(when (modulep! +wakatime)
  (package! wakatime-ui :recipe (:host github :repo "LemonBreezes/wakatime-ui.el"
                                 :branch "pad-modeline-string-right")))
(package! mlscroll)
