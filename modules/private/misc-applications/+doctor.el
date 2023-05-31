;;; private/misc-applications/+doctor.el -*- lexical-binding: t; -*-

(use-package! doctor
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-games-prefix
        "D" #'doctor))
