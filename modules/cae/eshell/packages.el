;; -*- no-byte-compile: t; -*-
;;; private/eshell/packages.el

(package! eat)
(package! eshell-bookmark)
(package! eshell-prompt-extras)
(package! tldr)

;; These packages have no `use-package!' blocks
(package! stutter :recipe (:host github :repo "Stebalien/stutter.el"))
