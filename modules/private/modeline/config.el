;;; private/modeline/config.el -*- lexical-binding: t; -*-

(use-package! minions
  :init
  (add-hook 'doom-first-buffer-hook #'minions-mode)
  :config
  (setq minions-hidden-modes
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
          eat-eshell-mode
          modern-c++-font-lock-mode))
  (setq minions-available-modes
        '(;; (abbrev-mode)
          (auto-fill-mode)
          (auto-revert-mode)
          (auto-revert-tail-mode)
          ;; (flyspell-mode)
          (spell-fu-mode)
          (font-lock-mode)
          (glasses-mode)
          ;; (overwrite-mode)
          (ruler-mode)
          (visual-line-mode)
          (display-fill-column-indicator-mode)))
  (setq minions-prominent-modes '(defining-kbd-macro
                                  projectile-mode
                                  flycheck-mode))
  (after! compile
    (setf (alist-get 'compilation-in-progress
                     (default-value 'minions-mode-line-modes))
          (propertize "[Compiling] "
	              'help-echo "Compiling; mouse-2: Goto Buffer"
                      'mouse-face 'mode-line-highlight
                      'local-map
                      (make-mode-line-mouse-map
                       'mouse-2
		       #'compilation-goto-in-progress-buffer))))
  (map! "<f9>" #'minions-minor-modes-menu))

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
