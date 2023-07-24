;;; private/misc-applications/daemons.el -*- lexical-binding: t; -*-

(use-package! daemons
  :defer t
  :when (eq system-type 'gnu/linux)
  :init
  (map! :map +misc-applications-system-map
        "u" #'daemons)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-system-map
      "u" "services"))
  :config
  (setq daemons-always-sudo t
        daemons-show-output-in-minibuffer t))
