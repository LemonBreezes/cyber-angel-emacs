;;; private/misc-applications/+bubbles.el -*- lexical-binding: t; -*-

(use-package! bubbles
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-games-prefix
        "b" #'bubbles)
  :config
  (map! :map bubbles-mode-map
        :n "RET" #'bubbles-plop))
