;;; private/misc-applications/+fireplace.el -*- lexical-binding: t; -*-

(use-package! fireplace
  :defer t
  :init
  (map! :map +misc-applications-eyecandy-map
        "f" #'fireplace)
  :config
  (map! :map fireplace-mode-map
        "<f6>" #'+fireplace-hydra/body))
