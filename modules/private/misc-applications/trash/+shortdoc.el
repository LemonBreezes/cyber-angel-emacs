;;; private/misc-applications/+shortdoc.el -*- lexical-binding: t; -*-

;; All the lookup packages got removed because ChatGPT essentially made them
;; obsolete.

;; built-in to Emacs 28 (or 29?)
(use-package! shortdoc
  :defer t
  :init
  (map! :map +misc-applications-lookup-map
        "s" #'shortdoc)
  :config
  (require 'posimacs-shortdocs))
