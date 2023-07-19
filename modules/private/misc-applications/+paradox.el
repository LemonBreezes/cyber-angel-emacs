;;; private/misc-applications/+paradox.el -*- lexical-binding: t; -*-

(use-package! paradox
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-system-prefix
        :desc "emacs packages" "p" #'paradox-list-packages)
  :config
  (paradox-enable))
