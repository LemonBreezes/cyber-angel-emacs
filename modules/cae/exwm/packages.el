;; -*- no-byte-compile: t; -*-
;;; private/exwm/packages.el

(package! exwm :recipe
  (:host github :repo "emacs-exwm/exwm"))
(package! exwm-mff)
(when (modulep! :editor evil +everywhere)
  (package! exwm-evil
    :recipe (:host github :repo "LemonBreezes/exwm-evil"))
  (package! exwm-firefox-evil))
(when (modulep! +i3bar)
  (package! i3bar :recipe
    (:host github :repo "Stebalien/i3bar.el")))
