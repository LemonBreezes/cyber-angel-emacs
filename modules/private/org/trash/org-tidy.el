;;; private/org/trash/org-tidy.el -*- lexical-binding: t; -*-

(use-package! org-tidy
  :defer t :init
  (add-hook 'org-mode-hook #'org-tidy-mode)
  :config
  (setq org-tidy-properties-inline-symbol "·"))
