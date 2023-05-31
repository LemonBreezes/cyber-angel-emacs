;;; private/debugger/config.el -*- lexical-binding: t; -*-

(when (and (modulep! :tools lsp)
           (modulep! :tools debugger +lsp))
  (map! "<f6>" #'dap-hydra)
  (after! dap-ui
    (remove-hook 'dap-ui-mode-hook #'dap-ui-controls-mode)
    (map! :map dap-ui-repl-mode-map
          [remap comint-delchar-or-maybe-eof] #'cae-debugger-quit-or-delete-or-send-eof))
  (after! dap-mode
    (setq dap-debug-restart-keep-session nil
          dap-auto-configure-features '(sessions locals breakpoints expressions tooltip))

    (after! dap-hydra
      (defhydra+ dap-hydra ()
        ("<f6>" nil "quit")
        ("R" cae-debugger-dap-kill-all-sessions-and-restart "Restart")))

    (when (modulep! :private corfu)
      (defun cae-debugger-dap-ui-repl-corfu-setup ()
        (add-to-list 'completion-at-point-functions #'cape-dabbrev))
      (add-hook 'dap-ui-repl-mode-hook #'cae-debugger-dap-ui-repl-corfu-setup)))

  (when (modulep! :lang cc +lsp)
    (add-transient-hook! 'c-mode-common-hook
      (require 'dap-cpptools)
      (dap-cpptools-setup))
    (when (modulep! :tools eval)
      (set-repl-handler! 'c++-mode #'cae-debugger-open-repl))))

(after! gud
  (setq gud-chdir-before-run nil)
  (add-hook 'gud-mode-hook #'gud-tooltip-mode))
(after! gdb-mi
  (setq gdb-show-main t
        gdb-many-windows t
        gdb-display-io-nopopup t
        gdb-show-changed-values t
        gdb-delete-out-of-scope t
        gdb-use-colon-colon-notation t
        gdb-restore-window-configuration-after-quit t)
  ;; For some reason, just binding this key in `gdb-frames-mode-map' doesn't
  ;; work for me.
  (map! :map comint-mode-map
        "RET" #'cae-debugger-gud-comint-send-input))
(after! projectile
  (map! :map projectile-command-map
        "@" #'projectile-run-gdb))
