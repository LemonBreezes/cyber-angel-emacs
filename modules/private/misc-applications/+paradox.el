;;; private/misc-applications/+paradox.el -*- lexical-binding: t; -*-

(use-package! paradox
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-system-prefix
        :desc "emacs packages" "p" #'paradox-list-packages)
  :config
  (paradox-enable)
  (map! :map paradox-menu-mode-map
        "<f6>" #'cae-paradox-menu-quick-help))
