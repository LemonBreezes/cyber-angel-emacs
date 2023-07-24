;;; private/misc-applications/+paradox.el -*- lexical-binding: t; -*-

(use-package! paradox
  :defer t
  :init
  (map! :map +misc-applications-system-map
        "p" #'paradox-list-packages)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-system-map
      "p" "emacs packages"))
  :config
  (paradox-enable)
  (map! :map paradox-menu-mode-map
        "<f6>" #'cae-paradox-menu-quick-help))
