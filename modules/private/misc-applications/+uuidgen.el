;;; private/misc-applications/+uuidgen.el -*- lexical-binding: t; -*-

(use-package! uuidgen
  :defer t
  :init
  (map! :map +misc-applications-insert-map
        "u" #'uuidgen))
