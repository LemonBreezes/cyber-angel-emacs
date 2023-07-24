;;; private/misc-applications/+password-generator.el -*- lexical-binding: t; -*-

(use-package! password-generator
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-insert-prefix
        "pc" #'password-generator-custom
        "ps" #'password-generator-simple
        "pt" #'password-generator-strong
        "pn" #'password-generator-numeric
        "pp" #'password-generator-paranoid
        "ph" #'password-generator-phonetic))
