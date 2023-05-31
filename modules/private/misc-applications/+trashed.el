;;; private/misc-applications/+trashed.el -*- lexical-binding: t; -*-

(use-package! trashed
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        "C-t" #'trashed)
  :config
  (advice-add #'trashed :around #'+trashed-revert-buffer-a)
  (add-hook 'trashed-mode-hook #'+trashed-hide-cursor-h))
