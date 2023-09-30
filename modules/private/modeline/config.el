;;; private/modeline/config.el -*- lexical-binding: t; -*-

(when (or (not (cae-display-graphic-p))
          (not (modulep! :ui modeline)))
  (remove-hook 'doom-after-init-hook #'doom-modeline-mode)
  (when (modulep! :editor evil)
    (setq evil-mode-line-format
          '(after . mode-line-frame-identification)))

  (use-package! anzu
    :after-call isearch-mode
    :defer t :init
    (global-set-key [remap query-replace] 'anzu-query-replace)
    (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
    (define-key isearch-mode-map [remap isearch-query-replace] #'anzu-isearch-query-replace)
    (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)
    :config
    (setq anzu-mode-lighter ""
          anzu-replace-threshold 50
          anzu-replace-to-string-separator " → "))

  (use-package! evil-anzu
    :when (modulep! :editor evil)
    :after-call evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight
    :config (global-anzu-mode +1))

  (use-package! minions
    :defer t :init
    (add-hook 'doom-after-init-hook #'minions-mode)
    :config
    (setq minions-mode-line-lighter "≡")
    (setq minions-demoted-modes
          '(abbrev-mode
            auto-fill-function
            eldoc-mode
            envrc-mode
            flycheck-mode
            flyspell-mode
            git-gutter+-mode
            git-gutter-mode
            git-timemachine-mode
            helm-mode
            helm-gtags-mode
            helm-projectile-mode
            highlight-indentation-mode
            highlight-numbers-mode
            highlight-parentheses-mode
            highlight-symbol-mode
            hs-minor-mode
            linum-mode
            nyan-mode
            projectile-mode
            rainbow-delimiters-mode
            smartparens-mode
            undo-tree-mode
            volatile-highlights-mode
            yas-minor-mode
            which-key-mode
            vi-tilde-fringe-mode
            page-break-lines-mode
            gcmh-mode
            general-override-mode
            editorconfig-mode
            better-jumper-local-mode
            persp-mode
            ws-butler-mode
            outline-minor-mode
            diff-hl-flydiff-mode
            copilot-mode
            git-auto-commit-mode
            hl-todo-mode
            whitespace-mode
            highlight-quoted-mode
            diff-hl-mode
            lsp-completion-mode
            lsp-diagnostics-mode
            lsp-mode
            lsp-modeline-diagnostics-mode
            lsp-modeline-code-actions-mode
            lsp-modeline-workspace-status-mode
            dtrt-indent-mode
            tree-sitter-mode
            dap-mode
            spell-fu-mode
            auto-sudoedit-mode
            hungry-delete-mode
            meow-insert-mode
            theme-magic-export-theme-mode
            eat-eshell-mode
            aggressive-indent-mode
            modern-c++-font-lock-mode
            better-jumper-mode
            undo-fu-session-mode
            rxt-mode
            dap-tooltip-mode
            dap-auto-configure-mode
            marginalia-mode
            +lsp-optimization-mode
            org-roam-db-autosync-mode
            dap-ui-many-windows-mode
            delete-selection-mode
            global-so-long-mode
            global-font-lock-mode
            meow-global-mode
            savehist-mode
            server-mode
            yas-global-mode
            auto-compression-mode
            auto-encryption-mode
            all-the-icons-completion-mode
            +popup-mode
            corfu-history-mode
            dirvish-override-dired-mode
            envrc-global-mode
            global-git-commit-mode
            save-place-mode
            recentf-mode
            mouse-wheel-mode
            minions-mode
            show-paren-mode
            global-hl-line-mode
            global-corfu-mode
            global-eldoc-mode
            tab-bar-mode
            repeat-mode
            dap-ui-mode
            column-number-mode
            eat-eshell-visual-command-mode
            transient-mark-mode
            treemacs-filewatch-mode
            line-number-mode
            treemacs-follow-mode
            smartparens-global-mode
            ws-butler-global-mode
            corfu-indexed-mode
            eros-mode
            gdb-many-windows
            file-name-shadow-mode
            corfu-popupinfo-mode
            undo-fu-mode
            treemacs-git-mode
            vertico-mode
            helm-mode
            winner-mode
            winum-mode
            undo-fu-session-global-mode
            treemacs-fringe-indicator-mode
            window-divider-mode
            electric-indent-mode
            topsy-mode
            hl-line-mode
            goggles-mode
            display-line-numbers-mode
            auto-save-mode
            corfu-mode
            auto-composition-mode
            override-global-mode
            font-lock-mode
            meow-mode
            shell-dirtrack-mode
            meow-normal-mode
            meow-motion-mode
            meow-keypad-mode
            highlight-changes-visible-mode
            rainbow-mode
            writegood-mode
            sqlup-mode))
    (setq minions-promoted-modes
          '(orgtbl-mode
            overwrite-mode
            auto-fill-mode
            auto-revert-mode
            auto-revert-tail-mode
            font-lock-mode
            highlight-changes-mode
            ruler-mode
            vline-mode))
    (setq minions-prominent-modes '(defining-kbd-macro
                                     ;;projectile-mode
                                     flycheck-mode
                                     flymake-mode
                                        ;persp-mode
                                     eat-eshell-mode
                                     envrc-mode
                                     ))
    (after! compilation
      (or (assq 'compilation-in-progress mode-line-modes)
          (add-to-list 'minions-mode-line-modes
                       (list 'compilation-in-progress
                             (propertize "[Compiling] "
                                         'help-echo "Compiling; mouse-2: Goto Buffer"
                                         'mouse-face 'mode-line-highlight
                                         'local-map
                                         (make-mode-line-mouse-map
                                          'mouse-2
                                          #'compilation-goto-in-progress-buffer)))))))

  (defalias 'cae-modeline-truncate-string (doom-rpartial #'truncate-string-to-width 30 nil nil t))
  (advice-add #'vc-git-mode-line-string :filter-return #'cae-modeline-truncate-string)

  (add-hook 'doom-first-file-hook #'column-number-mode)

  ;; I have this disabled because I never use it to scroll the window.
  (when (modulep! +pretty)
    (use-package! nyan-mode
      :when (cae-display-graphic-p)
      :defer t :init
      (add-hook 'doom-after-init-hook #'nyan-mode)
      :config
      (setq! nyan-bar-length 20
             nyan-minimum-window-width 20))))
