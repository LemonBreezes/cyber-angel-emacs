;;; private/misc-applications/+proced.el -*- lexical-binding: t; -*-

(use-package! proced
  :defer t
  :init
  (map! :leader :prefix +misc-applications-lists-prefix
        :desc "system processes" "s" #'proced)
  :config
  (setq proced-enable-color-flag t)
  (map! :map proced-mode-map
        "<f6>" #'+proced-hydra/body))
