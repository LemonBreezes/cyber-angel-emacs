;;; private/misc-applications/+password-generator.el -*- lexical-binding: t; -*-

(use-package! password-generator
  :defer t
  :init
  (map! :leader
        (:prefix +misc-applications-insert-prefix
         (:prefix ("p" . "password-generator")
          "c" #'password-generator-custom
          "s" #'password-generator-simple
          "t" #'password-generator-strong
          "n" #'password-generator-numeric
          "p" #'password-generator-paranoid
          "h" #'password-generator-phonetic))))
