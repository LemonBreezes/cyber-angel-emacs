;; -*- no-byte-compile: t; -*-
;;; cae/debugger/packages.el

(package! cae-lib :recipe (:host github :repo "LemonBreezes/cae-lib"))

(when (modulep! :tools lsp +eglot)
  (package! dape))
