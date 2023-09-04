;;; private/holy/restore-point.el -*- lexical-binding: t; -*-

(use-package! restore-point
    :defer t :init
    (add-hook 'doom-first-input-hook #'restore-point-mode)
    :config
    (setq rp/restore-point-commands
          (nconc
           '(beginning-of-buffer
             end-of-buffer
             mark-defun
             mark-page
             mark-paragraph
             mark-sexp
             mark-whole-buffer
             mark-work
             mwheel-scroll
             scroll-bar-drag
             scroll-bar-scroll-down
             scroll-bar-scroll-up
             scroll-bar-toolkit-scroll
             scroll-down-command
             scroll-other-window
             scroll-other-window-down
             scroll-up scroll-down
             scroll-up-command
             rp/point-ring-nav-previous)
           '(symbol-overlay-switch-forward
             symbol-overlay-switch-backward
             symbol-overlay-jump-next
             symbol-overlay-jump-prev
             recenter-top-bottom
             reposition-window
             er/expand-region
             eri/maximize-region
             eri/mark-block
             eri/expand-region
             eri/mark-line
             eri/contract-region
             eri/mark-outside-quotes
             eri/web-mode-element-parent
             eri/mark-inside-org-table-cell
             eri/mark-outside-org-table-cell
             eri/web-mode-element-parent-content
             er/mark-org-code-block
             er/mark-url
             er/mark-word
             er/mark-defun
             er/mark-email
             er/mark-symbol
             er/mark-comment
             er/mark-sentence
             er/mark-paragraph
             er/mark-org-parent
             er/mark-method-call
             er/mark-org-element
             er/mark-inside-pairs
             er/mark-inside-quotes
             er/mark-next-accessor
             er/mark-outside-pairs
             er/mark-text-sentence
             er/mark-outside-quotes
             er/mark-text-paragraph
             er/mark-org-element-parent
             er/mark-symbol-with-prefix
             cae-mark-comment
             View-scroll-half-page-forward
             View-scroll-half-page-backward
             chatgpt-shell-mark-at-point-dwim
             c-mark-function
             mark-whole-buffer)
           (and (featurep 'evil)
                '(evil-snipe-s
                  evil-snipe-S
                  evil-snipe-f
                  evil-snipe-F
                  evil-snipe-t
                  evil-snipe-T
                  evil-snipe-x
                  evil-snipe-X
                  evil-snipe-repeat
                  evil-snipe-repeat-reverse
                  evil-ex-search-forward
                  evil-ex-search-backward
                  evil-ex-search-next
                  evil-ex-search-previous
                  evil-ex-search-word-forward
                  evil-ex-search-word-backward
                  evil-ex-search-unbounded-word-forward
                  evil-ex-search-unbounded-word-backward
                  evil-goto-line
                  evil-jump-item
                  evil-goto-first-line
                  keyboard-escape-quit
                  evil-force-normal-state
                  evil-exit-visual-state
                  evil-normal-state
                  evil-inner-xml-attr
                  +evil:inner-url-txtobj
                  +evil:inner-any-quote
                  evil-indent-plus-i-indent-up
                  evil-indent-plus-i-indent-up-down
                  evil-indent-plus-i-indent
                  +evil:whole-buffer-txtobj
                  +evil:defun-txtobj
                  evilnc-inner-comment
                  evil-inner-arg
                  evil-inner-symbol
                  evil-inner-tag
                  evil-inner-back-quote
                  evil-inner-double-quote
                  evil-inner-single-quote
                  evil-inner-angle
                  evil-inner-angle
                  evil-inner-curly
                  evil-inner-curly
                  evil-textobj-anyblock-inner-block
                  evil-inner-bracket
                  evil-inner-bracket
                  evil-inner-paren
                  evil-inner-paren
                  evil-inner-paren
                  evil-inner-paragraph
                  evil-inner-sentence
                  evil-inner-WORD
                  evil-inner-word
                  evil-outer-xml-attr
                  +evil:outer-url-txtobj
                  +evil:outer-any-quote
                  evil-indent-plus-a-indent-up
                  evil-indent-plus-a-indent-up-down
                  evil-indent-plus-a-indent
                  +evil:whole-buffer-txtobj
                  +evil:defun-txtobj
                  evilnc-outer-commenter
                  evil-outer-arg
                  evil-a-symbol
                  evil-a-tag
                  evil-a-back-quote
                  evil-a-double-quote
                  evil-a-single-quote
                  evil-an-angle
                  evil-an-angle
                  evil-a-curly
                  evil-a-curly
                  evil-textobj-anyblock-a-block
                  evil-a-bracket
                  evil-a-bracket
                  evil-a-paren
                  evil-a-paren
                  evil-a-paren
                  evil-a-paragraph
                  evil-a-sentence
                  evil-a-WORD
                  evil-a-word
                  evil-scroll-up
                  evil-scroll-down
                  evil-scroll-left
                  evil-scroll-right
                  evil-scroll-line-up
                  evil-scroll-line-down
                  evil-scroll-page-down
                  evil-scroll-page-up
                  evil-forward-word-begin
                  evil-forward-WORD-begin
                  evil-forward-WORD-end
                  evil-forward-word-end
                  evil-backward-word-begin
                  evil-backward-WORD-begin
                  evil-backward-WORD-end
                  evil-backward-word-end
                  evil-forward-arg
                  evil-backward-arg
                  lispyville-backward-atom-begin
                  lispyville-forward-atom-begin
                  lispyville-backward-atom-end
                  lispyville-forward-atom-end
                  ))))
    (advice-add #'rp/restore-point-position :after #'deactivate-mark)
    ;; Restore point in the minibuffer.
    (defun cae-restore-point-h ()
      (when restore-point-mode
        (rp/cond-restore-point)))
    (defun cae-restore-point-enable-in-minibuffer-h ()
      (if restore-point-mode
          (progn (advice-add #'minibuffer-keyboard-quit :before #'rp/cond-restore-point)
                 (advice-remove #'keyboard-quit #'rp/cond-restore-point)
                 ;; Use `doom-escape-hook' instead of a `keyboard-quit' advice because that
                 ;; way we are certain this function is only called interactively.
                 (add-hook 'doom-escape-hook #'cae-restore-point-h -1)

                 ;; When we're using Evil, we also want to restore point when we
                 ;; exit visual state.
                 (advice-add #'evil-exit-visual-state :before #'rp/cond-restore-point))
        (advice-remove #'minibuffer-keyboard-quit #'rp/cond-restore-point)
        (advice-remove #'evil-exit-visual-state #'rp/cond-restore-point)
        (remove-hook 'doom-escape-hook #'cae-restore-point-h)))
    (add-hook 'restore-point-mode-hook #'cae-restore-point-enable-in-minibuffer-h))
