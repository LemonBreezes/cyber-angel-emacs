;;; private/debugger/config.el -*- lexical-binding: t; -*-

(when (and (modulep! :tools lsp)
           (not (modulep! :tools lsp +eglot))
           (modulep! :tools debugger +lsp))
  (after! dap-ui
    (remove-hook 'dap-ui-mode-hook #'dap-ui-controls-mode)
    (map! :map dap-ui-repl-mode-map
          [remap comint-delchar-or-maybe-eof] #'cae-debugger-quit-or-delete-or-send-eof))
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
    (when (modulep! :tools eval)
      (after! cc-mode
        (map! :map c-mode-base-map
              :localleader
              "h" #'cae-debugger-dap-hydra/body)
        (set-repl-handler! 'c++-mode #'cae-debugger-open-repl)
        (set-repl-handler! 'c-mode #'cae-debugger-open-repl)))))


;; Pass the `direnv' to `dap-mode' if no environment is specified.
(defun cae-dap-debug-pass-envrc (args)
  (when (length= (plist-get (car args) :environment) 0)
    (plist-put (car args) :environment
               (apply #'vector
                      (mapcar (lambda (s)
                                (let ((m (string-match "=" s)))
                                  (if m
                                      (list :name
                                            (substring-no-properties s 0 m)
                                            :value
                                            (substring-no-properties s (1+ m)
                                                                     (length s)))
                                    (list :name s
                                          :value ""))))
                              process-environment))))
  args)
(advice-add #'dap-start-debugging-noexpand :filter-args
            #'cae-dap-debug-pass-envrc)

(when (and (modulep! :tools lsp +eglot)
           (modulep! :tools debugger +lsp))
  (use-package! dape
    :defer t))

(after! gud
  (setq gud-chdir-before-run nil
        gud-highlight-current-line t))
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
(map! :leader
      :desc "GDB" "og" #'cae-debugger-run-or-pop-to-gdb)
