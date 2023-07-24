;;; private/misc-applications/picpocket.el -*- lexical-binding: t; -*-

(use-package! picpocket
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        "P" #'picpocket))
