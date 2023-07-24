;;; private/misc-applications/+dunnet.el -*- lexical-binding: t; -*-

(use-package! dunnet
  :defer t
  :init
  (map! :map +misc-applications-games-map
        "d" #'dunnet))
