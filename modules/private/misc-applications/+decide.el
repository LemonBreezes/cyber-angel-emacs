;;; private/misc-applications/+decide.el -*- lexical-binding: t; -*-

(use-package! decide
  :defer t
  :init
  (autoload 'decide-prefix-map "decide")
  (map! :leader
        :prefix +misc-applications-insert-prefix
        "d" #'decide-prefix-map))
