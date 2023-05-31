;;; private/misc-applications/somafm.el -*- lexical-binding: t; -*-

(use-package! somafm
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        "@" #'+somafm)
  :config
  (map! :map somafm-mode-map
        "<f6>" #'+somafm-hydra/body))
