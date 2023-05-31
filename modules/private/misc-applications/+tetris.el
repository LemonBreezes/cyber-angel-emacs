;;; private/misc-applications/+tetris.el -*- lexical-binding: t; -*-

(use-package! tetris
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-games-prefix
        "t" #'tetris
        :prefix +misc-applications-eyecandy-prefix
        "t" #'autotetris)
  :config
  (map! :map tetris-mode-map
        "<f6>" #'+tetris-hydra/body))
