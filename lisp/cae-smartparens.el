;;; lisp/cae-smartparens.el -*- lexical-binding: t; -*-

;; Make `smartparens' optional. See also `lisp/cae-hacks.el' for a few lines of
;; code that need to be ran before the `doom!' block to prevent errors that
;; happen when `smartparens' is disabled.
(unless (modulep! :config default +smartparens)
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
  (sp-local-pair 'org-mode "<<" ">>")

  ;; I prefer for `C-M-n' and `C-M-p' to never act like `sp-backward-up-sexp' or
  ;; `sp-up-sexp'.
  (setq sp-navigate-interactive-always-progress-point t))
