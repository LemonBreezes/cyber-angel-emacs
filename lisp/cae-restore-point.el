;;; lisp/cae-restore-point.el -*- lexical-binding: t; -*-

(use-package! restore-point
    :defer t :init
    (add-hook 'doom-first-input-hook #'restore-point-mode)
    :config
    (dolist (fn '(symbol-overlay-switch-forward
                  symbol-overlay-switch-backward
                  symbol-overlay-jump-next
                  symbol-overlay-jump-prev
                  recenter-top-bottom
                  reposition-window
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
                  View-scroll-half-page-forward
                  View-scroll-half-page-backward
                  avy-goto-line))
      (add-to-list 'rp/restore-point-commands fn))
    (advice-add #'rp/restore-point-position :after #'deactivate-mark)
    ;; Restore point in the minibuffer.
    (defun cae-restore-point-h ()
      (when (bound-and-true-p restore-point-mode)
        (rp/cond-restore-point)))
    (defun cae-restore-point-enable-in-minibuffer-h ()
      (if restore-point-mode
          (progn (advice-add #'minibuffer-keyboard-quit :before #'rp/cond-restore-point)
                 (advice-remove #'keyboard-quit #'rp/cond-restore-point)
                 ;; Use `doom-escape-hook' instead of a `keyboard-quit' advice because that
                 ;; way we are certain this function is only called interactively.
                 (add-hook 'doom-escape-hook #'cae-restore-point-h -1))
        (advice-remove #'minibuffer-keyboard-quit #'rp/cond-restore-point)
        (remove-hook 'doom-escape-hook #'cae-restore-point-h)))
    (add-hook 'restore-point-mode-hook #'cae-restore-point-enable-in-minibuffer-h))
