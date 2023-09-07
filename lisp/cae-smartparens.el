;;; lisp/cae-smartparens.el -*- lexical-binding: t; -*-

;; Make `smartparens' optional.
(unless (modulep! :config default +smartparens)
  ;; Check this out. This guy removes
  ;; https://git.sr.ht/~alternateved/dotemacs/tree/main/item/init.el#L404
  ;; I think you could achieve similar effects with puni (which does a bit too
  ;; much for my taste) or paredit (which supposedly works with non-lisps to
  ;; some regard), but I prefer those small utility functions
  (provide 'smartparens)
  (defalias 'sp-local-pair #'ignore)
  (defalias 'sp-pair #'ignore)
  (defalias 'sp-with-modes #'ignore)
  (defalias 'sp--syntax-ppss #'syntax-ppss)
  (defalias 'sp-point-in-comment
    (lambda (&optional pos) (nth 4 (syntax-ppss pos))))
  (defalias 'sp-point-in-string
    (lambda (&optional pos) (nth 3 (syntax-ppss pos))))
  (defalias 'sp-beginning-of-sexp
    (lambda (&optional arg) (goto-char (beginning-of-thing 'sexp))))
  (remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
  (when (modulep! :editor lispy)
    (after! lispy
      (remove-hook 'lispy-mode-hook #'turn-off-smartparens-mode)))
  (when (modulep! :term eshell)
    (after! eshell
      (remove-hook 'eshell-mode-hook #'smartparens-mode)))
  (when (modulep! :editor snippets)
    (remove-hook 'yas-before-expand-snippet-hook
                 #'+snippets--disable-smartparens-before-expand-h))

  ;; This is how we get curly braces working in C without `smartparens'.
  (setq electric-pair-pairs '((?\" . ?\")
                              (?\{ . ?\})
                              (?\[ . ?\]))
        electric-pair-inhibit-predicate #'electric-pair-conservative-inhibit
        electric-pair-open-newline-between-pairs t)
  (electric-pair-mode +1)
  (map! [remap newline] nil))

(when (modulep! :config default +smartparens)
  (after! smartparens
    (sp-local-pair 'org-mode "<<" ">>")

    ;; I prefer for `C-M-n' and `C-M-p' to never act like `sp-backward-up-sexp' or
    ;; `sp-up-sexp'.
    (setq sp-navigate-interactive-always-progress-point t)

    (when (and (modulep! :editor evil)
               (not (modulep! :editor lispy)))
      (use-package! evil-cleverparens
        :defer t :init
        (setq evil-cp-additional-movement-keys
              '(("M-l" . evil-cp-end-of-defun)
                ("M-h" . evil-cp-beginning-of-defun)))
        (add-hook 'prog-mode-hook 'evil-cleverparens-mode)
        :custom ((evil-cleverparens-use-additional-bindings nil)
                 (evil-cleverparens-use-s-and-S nil)
                 (evil-cleverparens-swap-move-by-word-and-symbol t))))))
