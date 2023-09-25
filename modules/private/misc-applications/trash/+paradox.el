;;; private/misc-applications/trash/+paradox.el -*- lexical-binding: t; -*-

;; This package works but is no longer maintained so it might have broken by the
;; time you read this. I personally didn't really use anything fancy this
;; package gave me. I prefer the built-in package menu because it's faster.

(use-package! paradox
  :defer t :init
  (map! :map +misc-applications-system-map
        "e" #'paradox-list-packages)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-system-map
      "e" "emacs packages"))
  :config
  (paradox-enable))
