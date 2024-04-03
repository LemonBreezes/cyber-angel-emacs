;; -*- no-byte-compile: t; -*-
;;; private/eshell/packages.el

(package! eat)
(package! eshell-bookmark)
(package! eshell-prompt-extras)
(package! tldr)

;; Used occasionally for `stutter-mode'.
(package! stutter :recipe (:host github :repo "Stebalien/stutter.el"))
