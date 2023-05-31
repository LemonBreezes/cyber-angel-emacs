;;; private/misc-applications/disk-usage.el -*- lexical-binding: t; -*-

(use-package! disk-usage
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        "d" #'disk-usage)
  :config
  (map! :map disk-usage-mode-map
        "<f6>" #'+disk-usage-hydra/body))
