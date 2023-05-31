;;; private/org/config.el -*- lexical-binding: t; -*-

(use-package! org-rich-yank
  :defer t
  :init
  (map! :map org-mode-map
        "C-M-y" #'cae-org-rich-yank))
