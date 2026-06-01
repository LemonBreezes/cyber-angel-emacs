;; -*- no-byte-compile: t; -*-
;;; cae/exwm/packages.el

(package! cae-lib :recipe (:host github :repo "LemonBreezes/cae-lib"))

(package! xelb :recipe
  (:host github :repo "emacs-exwm/xelb"))
(package! exwm :recipe
  (:host github :repo "emacs-exwm/exwm"))
(package! exwm-mff)
(when (modulep! :editor evil +everywhere)
  (package! exwm-evil
    :recipe (:host github :repo "LemonBreezes/exwm-evil"))
  (package! exwm-firefox-evil)
  (package! app-launcher :recipe (:host github :repo "SebastienWae/app-launcher")))
