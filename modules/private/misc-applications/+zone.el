;;; private/misc-applications/+zone.el -*- lexical-binding: t; -*-

(use-package! zone
  :defer-incrementally t
  :init
  (setq zone-programs [zone-rainbow])
  (zone-when-idle (* 5 60)))
