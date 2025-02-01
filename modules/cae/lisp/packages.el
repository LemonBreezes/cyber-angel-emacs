;; -*- no-byte-compile: t; -*-
;;; cae/lisp/packages.el

(package! cae-lib :recipe (:host github :repo "LemonBreezes/cae-lib"))

(when (modulep! :editor lispy)
  (package! lispy :recipe (:host github :repo "enzuru/lispy")))
(package! xr)
