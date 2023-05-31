;;; private/misc-applications/+dunnet.el -*- lexical-binding: t; -*-

(use-package! dunnet
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-games-prefix
        "d" #'dunnet))
