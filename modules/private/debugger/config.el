;;; private/debugger/config.el -*- lexical-binding: t; -*-

(when (and (modulep! :tools lsp)
           (modulep! :tools debugger +lsp))
  (map! "<f6>" #'dap-hydra)
  (after! dap-ui
    (remove-hook 'dap-ui-mode-hook #'dap-ui-controls-mode)
    (map! :map dap-ui-repl-mode-map
          [remap comint-delchar-or-maybe-eof] #'cae-debugger-quit-or-delete-or-send-eof))
  (after! dap-mode
    ;; Close the windows when the debugger is closed.
    (advice-add #'dap-disconnect :after #'cae-debugger-dap-quit-session-a)

    (setq dap-debug-restart-keep-session nil
          dap-auto-configure-features '(sessions locals breakpoints expressions tooltip))

    (after! dap-hydra
      (defhydra dap-hydra (:color pink :hint nil :foreign-keys run)
        "
^Stepping^          ^Switch^                 ^Breakpoints^         ^Debug^                     ^Eval
^^^^^^^^----------------------------------------------------------------------------------------------------------------
_n_: Next           _ss_: Session            _bb_: Toggle          _dd_: Debug                 _ee_: Eval
_i_: Step in        _st_: Thread             _bd_: Delete          _dr_: Debug recent          _er_: Eval region
_o_: Step out       _sf_: Stack frame        _ba_: Add             _dl_: Debug last            _es_: Eval thing at point
_c_: Continue       _su_: Up stack frame     _bc_: Set condition   _de_: Edit debug template   _ea_: Add expression.
_r_: Restart frame  _sd_: Down stack frame   _bh_: Set hit count   _ds_: Debug restart
_Q_: Disconnect     _sl_: List locals        _bl_: Set log message
_R_: Restart      _sb_: List breakpoints
                  _sS_: List sessions
"
        ("n" dap-next)
        ("i" dap-step-in)
        ("o" dap-step-out)
        ("c" dap-continue)
        ("r" dap-restart-frame)
        ("ss" dap-switch-session)
        ("st" dap-switch-thread)
        ("sf" dap-switch-stack-frame)
        ("su" dap-up-stack-frame)
        ("sd" dap-down-stack-frame)
        ("sl" dap-ui-locals)
        ("sb" dap-ui-breakpoints)
        ("sS" dap-ui-sessions)
        ("bb" dap-breakpoint-toggle)
        ("ba" dap-breakpoint-add)
        ("bd" dap-breakpoint-delete)
        ("bc" dap-breakpoint-condition)
        ("bh" dap-breakpoint-hit-condition)
        ("bl" dap-breakpoint-log-message)
        ("dd" dap-debug)
        ("dr" dap-debug-recent)
        ("ds" dap-debug-restart)
        ("dl" dap-debug-last)
        ("de" dap-debug-edit-template)
        ("ee" dap-eval)
        ("ea" dap-ui-expressions-add)
        ("er" dap-eval-region)
        ("es" dap-eval-thing-at-point)
        ("q" nil "quit" :color blue)
        ("R" cae-debugger-dap-kill-all-sessions-and-restart)
        ("Q" dap-disconnect :color red))))

  (when (modulep! :lang cc +lsp)
    (add-transient-hook! 'c-mode-common-hook
      (require 'dap-cpptools)
      (dap-cpptools-setup))
    (when (modulep! :tools eval)
      (set-repl-handler! 'c++-mode #'cae-debugger/open-repl))))

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
