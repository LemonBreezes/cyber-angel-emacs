;; -*- no-byte-compile: t; -*-
;;; cae/denote/packages.el

(package! cae-lib :recipe (:host github :repo "LemonBreezes/cae-lib"))

(package! denote)
(package! denote-explorer :recipe (:host github :repo "pprevos/denote-explore"))
(package! denote-menu)
