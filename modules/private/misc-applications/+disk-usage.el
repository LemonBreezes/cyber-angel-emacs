;;; private/misc-applications/disk-usage.el -*- lexical-binding: t; -*-

(use-package! disk-usage
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-system-prefix
        :desc "disk usage" "u" #'disk-usage)
  :config
  (map! :map disk-usage-mode-map
        "<f6>" #'+disk-usage-hydra/body))
