;; -*- no-byte-compile: t; -*-
;;; cae/debugger/packages.el

(when (and (modulep! :tools lsp +eglot)
           (modulep! :tools debugger +lsp))
  (package! dape))
