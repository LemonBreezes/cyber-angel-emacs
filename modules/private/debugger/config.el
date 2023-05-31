;;; private/debugger/config.el -*- lexical-binding: t; -*-

(when (and (modulep! :tools lsp)
           (modulep! :tools debugger +lsp))

  (after! dap-mode
    ;; Close the windows when the debugger is closed.
    (advice-add #'dap-disconnect :after #'cae-debugger-quit-session-a)

    (setq dap-debug-restart-keep-session nil
          dap-auto-configure-features '(sessions locals breakpoints expressions tooltip)))

  (when (modulep! :lang cc +lsp)
    (add-transient-hook! 'c-mode-common-hook
      (require 'dap-cpptools)
      (dap-cpptools-setup))
    (set-repl-handler! 'c++-mode #'+dap-mode/open-repl)))

(after! gdb-mi
  ;; For some reason, just binding this key in `gdb-frames-mode-map' doesn't
  ;; work for me.
  (map! :map comint-mode-map
        "RET" #'cae-debugger-gdb-select-frame))

;; These keybindings are normally installed after running `gdb' but I want them
;; earlier in case I want to set breakpoints before running `gdb'.
(load! "+gud-bindings")
