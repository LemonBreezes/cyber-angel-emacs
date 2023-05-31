;;; private/misc-applications/+snake.el -*- lexical-binding: t; -*-

(use-package! snake
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-games-prefix
        :desc "Snake" "s" #'snake)
  :config
  (map! :map snake-mode-map
        "<f6>" #'+snake-hydra/body))
