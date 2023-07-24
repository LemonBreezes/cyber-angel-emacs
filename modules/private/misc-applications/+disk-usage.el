;;; private/misc-applications/disk-usage.el -*- lexical-binding: t; -*-

(use-package! disk-usage
  :defer t
  :init
  (map! :map +misc-applications-system-map
        "d" #'disk-usage)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-system-map
      "d" "disk usage"))
  :config
  (map! :map disk-usage-mode-map
        "<f6>" #'+disk-usage-hydra/body))
