;;; private/debugger/config.el -*- lexical-binding: t; -*-

(when (and (modulep! :tools lsp)
           (modulep! :tools debugger +lsp))

  ;; Close the windows when the debugger is closed.
  (add-hook! #'dap-disconnect
    (ignore-errors
      (let ((ignore-window-parameters t))
        (cl-loop for buf being the buffers
                 when (string-match-p "gdb" (buffer-name buf)) do
                 (cae-hacks-always-yes-a #'doom-kill-buffer-and-windows buf)))
      (hydra-keyboard-quit)))

  ;; Doom keeps emitting errors when trying to retrieve previous debug
  ;; sessions. This is a workaround.
  (doom-store-clear "+debugger")

  (when (modulep! :lang cc +lsp)
    (add-transient-hook! 'c-mode-common-hook
      (require 'dap-cpptools)
      (dap-cpptools-setup))
    (set-repl-handler! 'c++-mode #'+dap-mode/open-repl)))
