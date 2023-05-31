;;; private/misc-applications/helm-linux-disks.el -*- lexical-binding: t; -*-

(use-package! helm-linux-disks
  :defer t
  :when (and (eq system-type 'gnu/linux)
             (or (modulep! :private helm)
                     (modulep! :completion helm)))
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        "D" #'helm-linux-disks))
