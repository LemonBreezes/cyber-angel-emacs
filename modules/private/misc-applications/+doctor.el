;;; private/misc-applications/+doctor.el -*- lexical-binding: t; -*-

(use-package! doctor
  :defer t
  :init
  (map! :map +misc-applications-games-map
        "D" #'doctor))
