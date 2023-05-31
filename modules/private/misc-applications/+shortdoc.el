;;; private/misc-applications/+shortdoc.el -*- lexical-binding: t; -*-

;; built-in to Emacs 28 (or 29?)
(use-package! shortdoc
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-lookup-prefix
        "s" #'shortdoc))
