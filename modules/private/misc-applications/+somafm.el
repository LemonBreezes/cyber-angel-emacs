;;; private/misc-applications/somafm.el -*- lexical-binding: t; -*-

(use-package! somafm
  :init
  (map! :map +misc-applications-external-apps-map
        "s" #'+somafm)
  :config
  (map! :map somafm-mode-map
        "<f6>" #'+somafm-hydra/body))
