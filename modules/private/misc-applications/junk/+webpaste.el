;;; private/misc-applications/+webpaste.el -*- lexical-binding: t; -*-

(use-package! webpaste
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        "W" #'webpaste-paste-buffer-or-region))
