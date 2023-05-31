;;; private/misc-applications/+flames-of-freedom.el -*- lexical-binding: t; -*-

(use-package! flames-of-freedom
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-eyecandy-prefix
        "F" #'flames-of-freedom))
