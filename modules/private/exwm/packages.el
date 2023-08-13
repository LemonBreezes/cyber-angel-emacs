;; -*- no-byte-compile: t; -*-
;;; private/exwm/packages.el

(package! exwm :recipe
  (:host github :repo "ch11ng/exwm"))
(package! exwm-edit)
(package! exwm-mff)
(when (modulep! :editor evil +everywhere)
  (package! exwm-evil
    :recipe (:host github :repo "LemonBreezes/exwm-evil"))
  (package! exwm-firefox-evil))
(package! app-launcher :recipe
  (:host github :repo "SebastienWae/app-launcher"))
(when (modulep! +notifications)
  (package! ednc))
