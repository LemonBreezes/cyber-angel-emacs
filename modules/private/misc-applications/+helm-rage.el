;;; private/misc-applications/+helm-rage.el -*- lexical-binding: t; -*-

(use-package! helm-rage
  :defer t
  :when (or (modulep! :private helm)
            (modulep! :completion helm))
  :init
  (map! :map +misc-applications-insert-map
        "r" #'helm-rage))
