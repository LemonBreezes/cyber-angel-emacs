;;; private/misc-applications/+webpaste.el -*- lexical-binding: t; -*-

;; Nowadays I use 0x0 instead.

(use-package! webpaste
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        "W" #'webpaste-paste-buffer-or-region))
