;;; private/misc-applications/+wttrin.el -*- lexical-binding: t; -*-

(use-package! wttrin
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        "w" #'wttrin))
