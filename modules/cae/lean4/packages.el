;; -*- no-byte-compile: t; -*-
;;; lang/lean4/packages.el

(package! cae-lib :recipe (:host github :repo "LemonBreezes/cae-lib"))

(package! lean4-mode
  :recipe (:host github
	   :repo "leanprover/lean4-mode"
	   :files ("*.el" "data")))
