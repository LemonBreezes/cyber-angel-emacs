;;; private/misc-applications/+lorem-ipsum.el -*- lexical-binding: t; -*-

(use-package! lorem-ipsum
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-insert-prefix
        "ll" #'lorem-ipsum-insert-list
        "lp" #'lorem-ipsum-insert-paragraphs
        "ls" #'lorem-ipsum-insert-sentences))
