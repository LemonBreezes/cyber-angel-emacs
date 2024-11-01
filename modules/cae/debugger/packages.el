;; -*- no-byte-compile: t; -*-
;;; cae/debugger/packages.el

(package! cae-lib :recipe (:host github :repo "LemonBreezes/cae-lib"))

(when (and (modulep! :tools lsp +eglot)
           (modulep! :tools debugger +lsp))
  (package! dape))
