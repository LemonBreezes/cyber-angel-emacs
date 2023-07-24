;;; private/misc-applications/+password-generator.el -*- lexical-binding: t; -*-

(use-package! password-generator
  :defer t
  :init
  (map! :map +misc-applications-insert-map
        (:prefix "p"
         "c" #'password-generator-custom
         "s" #'password-generator-simple
         "t" #'password-generator-strong
         "n" #'password-generator-numeric
         "p" #'password-generator-paranoid
         "h" #'password-generator-phonetic))
  (after! which-key
    (which-key-add-keymap-based-replacements
      +misc-applications-insert-map
      "p" "password-generator"
      "pc" "custom"
      "ps" "simple"
      "pt" "strong"
      "pn" "numeric"
      "pp" "paranoid"
      "ph" "phonetic")))
