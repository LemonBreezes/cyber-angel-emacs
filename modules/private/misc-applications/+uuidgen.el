;;; private/misc-applications/+uuidgen.el -*- lexical-binding: t; -*-

(use-package! uuidgen
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-insert-prefix
        "u" #'uuidgen))
