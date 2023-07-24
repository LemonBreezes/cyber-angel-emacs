;;; private/misc-applications/+lorem-ipsum.el -*- lexical-binding: t; -*-

(use-package! lorem-ipsum
  :defer t
  :init
  (map! :leader
        (:prefix +misc-applications-insert-prefix
         (:prefix ("l" . "lorem-ipsum")
          "l" #'lorem-ipsum-insert-list
          "p" #'lorem-ipsum-insert-paragraphs
          "s" #'lorem-ipsum-insert-sentences))))
