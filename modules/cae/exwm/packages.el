;; -*- no-byte-compile: t; -*-
;;; cae/exwm/packages.el

(package! cae-lib :recipe (:host github :repo "LemonBreezes/cae-lib"))

(package! xelb :recipe
  (:host github :repo "emacs-exwm/xelb"))
(package! exwm :recipe
  (:host github :repo "emacs-exwm/exwm"))
(package! exwm-mff)
