;;; private/misc-applications/+fireplace.el -*- lexical-binding: t; -*-

(use-package! fireplace
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-eyecandy-prefix
        "f" #'fireplace)
  :config
  (map! :map fireplace-mode-map
        "<f6>" #'+fireplace-hydra/body))
