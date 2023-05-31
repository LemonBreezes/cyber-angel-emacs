;;; private/misc-applications/+zone.el -*- lexical-binding: t; -*-

(use-package! zone
  :defer-incrementally t
  :init
  (map! :leader
        :prefix +misc-applications-eyecandy-prefix
        "z" #'zone-choose)
  :config
  ;; zone-nyan
  ;; zone-rainbow
  ;; zone-pgm-md5
  (zone-when-idle (* 5 60)))
