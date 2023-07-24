;;; private/misc-applications/+snow.el -*- lexical-binding: t; -*-

(use-package! snow
  :defer t
  :init
  (map! :map +misc-applications-eyecandy-map
        "s" #'snow))
