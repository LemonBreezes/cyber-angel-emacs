;;; private/misc-applications/+helm-rage.el -*- lexical-binding: t; -*-

(use-package! helm-linux-disks
  :defer t
  :when (or (modulep! :private helm)
            (modulep! :completion helm))
  :init
  (map! :leader
        :prefix +misc-applications-insert-prefix
        :desc "disks" "D" #'helm-linux-disks))
