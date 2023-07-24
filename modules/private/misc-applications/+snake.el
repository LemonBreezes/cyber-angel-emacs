;;; private/misc-applications/+snake.el -*- lexical-binding: t; -*-

(use-package! snake
  :defer t
  :init
  (map! :map +misc-applications-games-map
        "s" #'snake)
  :config
  (map! :map snake-mode-map
        "<f6>" #'+snake-hydra/body))
