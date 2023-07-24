;;; private/misc-applications/+flames-of-freedom.el -*- lexical-binding: t; -*-

(use-package! flames-of-freedom
  :defer t
  :init
  (map! :map +misc-applications-eyecandy-map
        "F" #'flames-of-freedom-default))
