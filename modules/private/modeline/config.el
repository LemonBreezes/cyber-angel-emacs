;;; private/modeline/config.el -*- lexical-binding: t; -*-

(use-package! minions
  :init
  (add-hook 'doom-first-buffer-hook #'minions-mode)
  :config
  (setq minions-hidden-modes
        '(abbrev-mode
          auto-fill-function
          eldoc-mode
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
                                  projectile-mode))

(after! compile
    (setq-default minions-mode-line-modes
                  (cons
                   (list
                    'compilation-in-progress
                    (propertize "[Compiling] "
                                'help-echo "Compiling; mouse-2: Goto Buffer"
                                'mouse-face 'mode-line-highlight
                                'local-map
                                (make-mode-line-mouse-map
                                 'mouse-2
                                 #'compilation-goto-in-progress-buffer)))
                   (default-value 'minions-mode-line-modes))))

  (minions-mode +1))

(column-number-mode +1)

(map! "<f9>" #'minions-minor-modes-menu)

;; (advice-add 'create-file-buffer :override #'cae-modeline-create-file-buffer)

(use-package! nyan-mode
  :when (modulep! +pretty)
  :defer-incrementally t
  ;; (setq nyan-animate-nyancat t
  ;;       nyan-wavy-trail t
  ;;       nyan-bar-length 16
  ;;       nyan-cat-face-number 3
  ;;       nyan-animation-frame-interval 0.1
  ;;       nyan-minimum-window-width 80
  ;;       nyan-mode-line nil)
  :config
  (nyan-mode +1))

(use-package! parrot
  :when (modulep! +pretty)
  :defer-incrementally t
  :init
  :config
  (unwind-protect
      (progn (advice-add #'parrot-start-animation :override #'ignore)
             (parrot-mode +1))
    (setq! parrot-animate 'hide-static
           parrot-rotate-animate-after-rotation nil
           parrot-num-rotations 10
           parrot-animate-on-load nil
           parrot-party-on-magit-push t
           parrot-party-on-org-todo-states '("DONE")
           parrot-type 'nyan)
    (advice-remove #'parrot-start-animation #'ignore)))
