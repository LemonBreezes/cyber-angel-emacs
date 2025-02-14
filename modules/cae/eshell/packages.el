;; -*- no-byte-compile: t; -*-
;;; cae/eshell/packages.el

(package! cae-lib :recipe (:host github :repo "LemonBreezes/cae-lib"))

(package! eat)
(package! eshell-bookmark)
(package! eshell-prompt-extras)
(package! tldr)
(package! eshell-atuin)
(package! detached)

;; Optional but useful for detached.
(package! alert)
