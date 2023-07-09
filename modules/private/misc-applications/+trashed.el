;;; private/misc-applications/+trashed.el -*- lexical-binding: t; -*-

(use-package! trashed
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-lists-prefix
        :desc "trash files" "t" #'trashed)
  :config
  (advice-add #'trashed :around #'+trashed-revert-buffer-a)
  (add-hook 'trashed-mode-hook #'+trashed-hide-cursor-h)
  (map! :map trashed-mode-map
        "<f6>" #'+trashed-hydra/body)
  (add-hook 'trashed-mode-hook #'doom-mark-buffer-as-real-h))
