;;; cae/haskell/config.el -*- lexical-binding: t; -*-

(use-package! consult-hoogle
  :defer t :init
  (after! haskell-mode
    (map! :map haskell-mode-map
          :localleader
          "g" #'consult-hoogle
          "G" #'consult-hoogle-project)))
