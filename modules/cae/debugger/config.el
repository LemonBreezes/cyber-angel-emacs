;;; private/debugger/config.el -*- lexical-binding: t; -*-

(when (and (modulep! :tools lsp)
           (modulep! :tools debugger +lsp))
  (after! dap-ui
    (remove-hook 'dap-ui-mode-hook #'dap-ui-controls-mode)
    (map! :map dap-ui-repl-mode-map
          [remap comint-delchar-or-maybe-eof] #'cae-debugger-quit-or-delete-or-send-eof
          "<f6>" #'cae-debugger-dap-hydra/body))
  (after! dap-mode
    (remove-hook 'dap-stopped-hook #'+dap-running-session-mode)
    (setq dap-debug-restart-keep-session nil
          dap-auto-configure-features '(sessions locals breakpoints
                                        expressions tooltip))

    (when (modulep! :completion corfu)
      (defun cae-debugger-dap-ui-repl-corfu-setup ()
        (add-to-list 'completion-at-point-functions #'cape-dabbrev))
      (add-hook 'dap-ui-repl-mode-hook #'cae-debugger-dap-ui-repl-corfu-setup)))

  (when (modulep! :lang cc +lsp)
    (add-transient-hook! 'c-mode-common-hook
      (require 'dap-cpptools)
      (dap-cpptools-setup))
    (when (modulep! :tools eval)
      (after! cc-mode
        (set-repl-handler! 'c++-mode #'cae-debugger-open-repl)
        (set-repl-handler! 'c-mode #'cae-debugger-open-repl)))))

(after! gud
  (setq gud-chdir-before-run nil))
(after! gdb-mi
  (setq gdb-show-main nil
        gdb-many-windows nil
        gdb-display-io-nopopup nil
        gdb-show-changed-values t
        gdb-delete-out-of-scope t
        gdb-use-colon-colon-notation t
        gdb-restore-window-configuration-after-quit t
        gdb-debuginfod-enable nil
        gdb-debuginfod-enable-setting nil
        gdb-display-io-buffer nil))
(map! :leader "od" #'cae-debugger-run-or-pop-to-gdb)
