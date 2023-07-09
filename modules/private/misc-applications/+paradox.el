;;; private/misc-applications/+paradox.el -*- lexical-binding: t; -*-

(use-package! paradox
  :after package
  :init
  (map! :leader
        :prefix +misc-applications-lists-prefix
        :desc "emacs packages" "p" #'paradox-list-packages)
  :config
  (paradox-enable))
