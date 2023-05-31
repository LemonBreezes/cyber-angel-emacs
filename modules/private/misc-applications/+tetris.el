;;; private/misc-applications/+tetris.el -*- lexical-binding: t; -*-

(use-package! tetris
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-games-prefix
        "t" #'tetris))
