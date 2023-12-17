;; -*- no-byte-compile: t; -*-
;;; private/modeline/packages.el

(package! minions)
(when (modulep! +pretty)
  (package! nyan-mode))
(package! anzu)
(when (modulep! :editor evil)
  (package! evil-anzu))
(when (modulep! +wakatime)
  (package! wakatime-ui :recipe (:host github :repo "Artawower/wakatime-ui.el")))
