;;; lisp/cae-mc.el -*- lexical-binding: t; -*-

(dolist (cmd '(doom/delete-backward-word
                   doom/forward-to-last-non-comment-or-eol
                   mark-sexp
                   eros-eval-last-sexp
                   eval-last-sexp
                   cae-eval-last-sexp))
      (add-to-list 'mc/cmds-to-run-for-all cmd))
    (dolist (cmd '(+workspace/new +workspace/load +workspace/save
                   +workspace/cycle +workspace/other +workspace/delete
                   +workspace/rename +workspace/display +workspace/new-named
                   +workspace/swap-left +workspace/switch-to
                   +workspace/swap-right +workspace/switch-left
                   +workspace/switch-to-0 +workspace/switch-to-1
                   +workspace/switch-to-2 +workspace/switch-to-3
                   +workspace/switch-to-4 +workspace/switch-to-5
                   +workspace/switch-to-6 +workspace/switch-to-7
                   +workspace/switch-to-8 +workspace/kill-session
                   +workspace/switch-right +workspace/switch-to-final
                   +workspace/restore-last-session +workspace/kill-session-and-quit
                   +workspace/close-window-or-workspace read-only-mode))
      (add-to-list 'mc/cmds-to-run-once cmd))
