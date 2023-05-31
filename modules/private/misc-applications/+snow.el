;;; private/misc-applications/+snow.el -*- lexical-binding: t; -*-

(use-package! snow
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-eyecandy-prefix
        "s" #'snow))
