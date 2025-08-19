;; -*- no-byte-compile: t; -*-
;;; cae/eshell/packages.el

(package! cae-lib :recipe (:host github :repo "LemonBreezes/cae-lib"))

(package! eat :recipe (:host codeberg :repo "akib/emacs-eat"
                       :files ("*" "integration" "term" "terminfo")))
(package! eshell-bookmark)
(package! eshell-prompt-extras)
(package! eshell-atuin)
