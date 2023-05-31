;;; private/misc-applications/+snake.el -*- lexical-binding: t; -*-

(use-package! snake
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-games-prefix
        "s" #'snake))
