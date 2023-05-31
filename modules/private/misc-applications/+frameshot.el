;;; private/misc-applications/+frameshot.el -*- lexical-binding: t; -*-

(use-package! frameshot
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        "F" #'+frameshot-take)
  :config
  (setopt frameshot-config `((output . ,doom-picture-dir))))
