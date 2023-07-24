;;; private/misc-applications/+decide.el -*- lexical-binding: t; -*-

(use-package! decide
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-insert-prefix
        "d" #'decide-prefix-map))
