;;; private/debugger/config.el -*- lexical-binding: t; -*-

(when (and (modulep! :tools lsp)
           (modulep! :tools debugger +lsp))
  (after! dap-mode
    (remove-hook 'dap-stopped-hook #'+dap-running-session-mode)
    (map! :map dap-mode-map
          "C-S-d" #'dap-hydra))
  (when (modulep! :lang cc +lsp)
    (add-transient-hook! 'c-mode-common-hook
      (require 'dap-cpptools)
      (dap-cpptools-setup))))
