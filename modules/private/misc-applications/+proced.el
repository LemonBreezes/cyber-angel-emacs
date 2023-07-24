;;; private/misc-applications/+proced.el -*- lexical-binding: t; -*-

(use-package! proced
  :defer t
  :config
  (setq proced-enable-color-flag t)
  (map! :map proced-mode-map
        "<f6>" #'+proced-hydra/body))
