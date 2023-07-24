;;; private/misc-applications/+lorem-ipsum.el -*- lexical-binding: t; -*-

(use-package! lorem-ipsum
  :defer t
  :init
  (map! :map +misc-applications-insert-map
        (:prefix "l"
         "l" #'lorem-ipsum-insert-list
         "p" #'lorem-ipsum-insert-paragraphs
         "s" #'lorem-ipsum-insert-sentences))
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-insert-map
      "l" "lorem-ipsum"
      "l l" "lorem-ipsum-insert-list"
      "l p" "lorem-ipsum-insert-paragraphs"
      "l s" "lorem-ipsum-insert-sentences")))
