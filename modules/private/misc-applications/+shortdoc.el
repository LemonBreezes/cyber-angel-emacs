;;; private/misc-applications/+shortdoc.el -*- lexical-binding: t; -*-

;; built-in to Emacs 28 (or 29?)
(use-package! shortdoc
  :defer t
  :init
  (map! :map +misc-applications-lookup-map
        "s" #'shortdoc)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-lookup-map
      "s" "shortdoc"))
  :config
  (require 'posimacs-shortdocs))
