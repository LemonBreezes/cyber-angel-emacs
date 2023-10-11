;;; trash/cae-evil.el -*- lexical-binding: t; -*-

(use-package! evil-textobj-synax
  :defer t :init
  (after! evil
    (map! :map evil-inner-text-objects-map
          "h" #'evil-i-syntax
          :map evil-outer-text-objects-map
          "h" #'evil-a-syntax)))
