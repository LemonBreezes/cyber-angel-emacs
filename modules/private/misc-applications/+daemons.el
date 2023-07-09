;;; private/misc-applications/daemons.el -*- lexical-binding: t; -*-

(use-package! daemons
  :defer t
  :when (eq system-type 'gnu/linux)
  :init
  (map! :leader
        :prefix +misc-applications-lists-prefix
        :desc "daemons" "d" #'daemons)
  :config
  (setq daemons-always-sudo t
        daemons-show-output-in-minibuffer t))
