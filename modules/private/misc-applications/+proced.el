;;; private/misc-applications/+proced.el -*- lexical-binding: t; -*-

(use-package! proced
  :defer t
  :init
  (map! :leader :prefix +misc-applications-prefix
        "p" #'proced)
  :config
  (setq-default proced-auto-update-flag t)
  (setq-default proced-auto-update-interval 1))
