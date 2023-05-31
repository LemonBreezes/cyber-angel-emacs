;;; private/misc-applications/+proced.el -*- lexical-binding: t; -*-

(use-package! proced
  :defer t
  :init
  (map! :leader :prefix +misc-applications-prefix
        "p" #'proced
        "P" #'list-processes)
  :config
  (setq proced-enable-color-flag t))
