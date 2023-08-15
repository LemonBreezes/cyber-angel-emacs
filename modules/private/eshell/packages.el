;; -*- no-byte-compile: t; -*-
;;; private/eshell/packages.el

(package! eat)
(package! eshell-bookmark)
(package! eshell-prompt-extras :recipe
  (:host github :repo "LemonBreezes/eshell-prompt-extras"
   :branch "enable-lexical-binding"))
(package! eshell-syntax-highlighting)
(disable-packages! fish-completion)
;;(package! detached)
