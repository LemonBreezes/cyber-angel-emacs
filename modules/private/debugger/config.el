;;; private/debugger/config.el -*- lexical-binding: t; -*-

(when (and (modulep! :tools lsp)
           (modulep! :tools debugger +lsp))

  (after! dap-mode
    ;; Close the windows when the debugger is closed.
    (advice-add #'dap-disconnect :after #'cae-debugger-quit-session-a)

    (map! :map lsp-mode-map
          "C-M-S-d" #'dap-debug
          "C-M-S-r" #'cae-debugger-kill-all-sessions-and-restart
          "C-M-S-q" #'dap-disconnect
          "C-M-S-b" #'dap-breakpoint-toggle
          "C-M-S-h" #'dap-hydra
          (:prefix ("C-S-d" . "debug")
           :desc "DAP Hydra" "h"                     #'dap-hydra
           ;; :desc "Debug start" "s"                #'dap-debug
           :desc "Step over" "n"                     #'dap-next
           :desc "Step in" "i"                       #'dap-step-in
           :desc "Step out" "o"                      #'dap-step-out
           :desc "Continue" "c"                      #'dap-continue
           :desc "Restart frame" "r"                 #'dap-restart-frame
           (:prefix ("s" . "show")
            :desc "Switch session" "s"               #'dap-switch-session
            :desc "Switch thread" "t"                #'dap-switch-thread
            :desc "Switch stack frame" "f"           #'dap-switch-stack-frame
            :desc "Move up stack frame" "u"          #'dap-up-stack-frame
            :desc "Move down stack frame" "d"        #'dap-down-stack-frame
            :desc "Show local variables" "l"         #'dap-ui-locals
            :desc "Show breakpoints" "b"             #'dap-ui-breakpoints
            :desc "Show sessions" "S"                #'dap-ui-sessions)
           (:prefix ("b" . "breakpoints")
            :desc "Toggle breakpoint" "b"            #'dap-breakpoint-toggle
            :desc "Add breakpoint" "a"               #'dap-breakpoint-add
            :desc "Delete breakpoint" "d"            #'dap-breakpoint-delete
            :desc "Set breakpoint condition" "c"     #'dap-breakpoint-condition
            :desc "Set breakpoint hit condition" "h" #'dap-breakpoint-hit-condition
            :desc "Set breakpoint log message" "l"   #'dap-breakpoint-log-message)
           (:prefix ("d" . "debug")
            :desc "Debug" "d"                        #'dap-debug
            :desc "Debug recent" "r"                 #'dap-debug-recent
            :desc "Debug restart" "s"                #'dap-debug-restart
            :desc "Debug last" "l"                   #'dap-debug-last
            :desc "Edit template" "e"                #'dap-debug-edit-template)
           (:prefix ("e" . "eval")
            :desc "Eval" "e"                         #'dap-eval
            :desc "Add expression" "a"               #'dap-ui-expressions-add
            :desc "Eval region" "r"                  #'dap-eval-region
            :desc "Eval thing at point" "s"          #'dap-eval-thing-at-point)
           :desc "Disconnect" "q"                   #'dap-disconnect))

    (setq dap-debug-restart-keep-session nil
          dap-auto-configure-features '(sessions locals breakpoints expressions tooltip)))

  (when (modulep! :lang cc +lsp)
    (add-transient-hook! 'c-mode-common-hook
      (require 'dap-cpptools)
      (dap-cpptools-setup))
    (set-repl-handler! 'c++-mode #'+dap-mode/open-repl)))
