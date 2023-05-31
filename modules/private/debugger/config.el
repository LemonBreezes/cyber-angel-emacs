;;; private/debugger/config.el -*- lexical-binding: t; -*-

(when (and (modulep! :lang cc +lsp)
           (modulep! :tools lsp)
           (modulep! :tools debugger +lsp))
  (add-transient-hook! 'c-mode-common-hook
    (require 'dap-cpptools)
    (dap-cpptools-setup)))
