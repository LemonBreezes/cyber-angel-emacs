;;; trash/config-use-package-blocks.el -*- lexical-binding: t; -*-

(use-package! aggressive-indent
  :disabled t
  :defer t :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode) ;See my `lisp'
                                        ;module.
  (add-hook 'c-mode-common-hook #'aggressive-indent-mode)
  :config
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (bound-and-true-p lsp-mode)
         (or (and lsp-enable-on-type-formatting
                  (lsp--capability "documentOnTypeFormattingProvider"))
             (and lsp-enable-indentation
                  (lsp--capability "documentRangeFormattingProvider")))))
  (dolist (command '(lsp-format-buffer
                     lsp-format-region
                     lsp-organize-imports
                     lsp-organize-imports-remove-unused
                     prog-fill-reindent-defun
                     indent-pp-sexp
                     save-buffer
                     indent-for-tab-command))
    (add-to-list 'aggressive-indent-protected-commands command))
  (add-to-list 'aggressive-indent-dont-indent-if '(bound-and-true-p lispy-mode)))
