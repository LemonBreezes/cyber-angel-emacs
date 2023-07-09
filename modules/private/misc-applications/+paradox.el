;;; private/misc-applications/+paradox.el -*- lexical-binding: t; -*-

(use-package! paradox
  :after package
  :init
  (map! :prefix +misc-applications-lists-prefix
        "emacs packages" #'paradox-list-packages)
  :config
  )
