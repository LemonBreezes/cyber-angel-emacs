;;; private/modeline/config.el -*- lexical-binding: t; -*-

(minions-mode 1)
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
        ))
(setq minions-available-modes
      '(;; (abbrev-mode)
        (auto-fill-mode)
        (auto-revert-mode)
        (auto-revert-tail-mode)
        ;; (flyspell-mode)
        (spell-fu-mode)
        (flycheck-mode)
        (flycheck-mode)
        (font-lock-mode)
        (highlight-changes-mode)
        ;; (overwrite-mode)
        (ruler-mode)
        (rainbow-mode)
        (visual-line-mode)
        (display-fill-column-indicator-mode)))

(add-hook 'c-mode-common-hook #'cae-modeline-minions-c-setup)
(add-hook 'emacs-lisp-mode-hook #'cae-modeline-minions-elisp-setup)
(map! "<f9>" #'minions-minor-modes-menu)

(advice-add 'create-file-buffer :around #'cae-modeline-create-file-buffer-a)
