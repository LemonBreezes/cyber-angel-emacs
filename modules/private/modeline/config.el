;;; private/modeline/config.el -*- lexical-binding: t; -*-

(use-package! minions
  :defer t
  :init
  (add-hook 'doom-first-buffer-hook #'minions-mode)
  :config
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
                                   projectile-mode
                                   flycheck-mode
                                   flymake-mode
                                   persp-mode
                                   eat-eshell-mode))
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
                                        #'compilation-goto-in-progress-buffer))))))
  (map! "<f9>" #'minions-minor-modes-menu))

(defalias 'cae-modeline-truncate-string (doom-rpartial #'truncate-string-to-width 30 nil nil t))
(advice-add #'vc-git-mode-line-string :filter-return #'cae-modeline-truncate-string)

(add-hook 'doom-first-file-hook #'column-number-mode)

(when (modulep! +pretty)
  (use-package! nyan-mode
    :when (display-graphic-p)
    :init
    (add-hook 'doom-first-buffer-hook #'nyan-mode)
    :config
    (setopt nyan-bar-length 20
      nyan-minimum-window-width 20))

  ;; This block might seem crazy, but it's how I've gotten parrot mode to work
  ;; and be silent on startup.
  (use-package! parrot
    :after magit
    :defer t :init
    (map! "C-!" #'parrot-rotate-next-word-at-point)
    :config
    (setopt parrot-animate 'hide-static
      parrot-rotate-animate-after-rotation nil
      parrot-num-rotations 10
      parrot-animate-on-load nil
      parrot-party-on-magit-push t
      parrot-party-on-org-todo-states '("DONE")
      parrot-type 'nyan)
    (parrot-mode +1)
    (setq parrot-rotate-start-bound-regexp "[\]\[[:space:](){}<>]"
      parrot-rotate-end-bound-regexp "[\]\[[:space:](){}<>]")
    (add-to-list 'parrot-rotate-dict '(:rot ("add-hook" "remove-hook")))
    (add-to-list 'parrot-rotate-dict '(:rot ("add-hook!" "remove-hook!")))
    (add-to-list 'parrot-rotate-dict '(:rot ("advice-add" "advice-remove")))
    (add-to-list 'parrot-rotate-dict '(:rot ("defadvice!" "undefadvice!")))
    (add-to-list 'parrot-rotate-dict '(:rot ("cae-keyboard-remap"
                                              "cae-keyboard-remap-to-strings"
                                              "cae-keyboard-strings")))
    (add-to-list 'parrot-rotate-dict '(:rot ("kbd" "cae-keyboard-kbd")))
    (add-to-list 'parrot-rotate-dict '(:rot ("+log" "message")))
    (add-to-list 'parrot-rotate-dict '(:rot ("backtrace!" "unbacktrace!")))))
