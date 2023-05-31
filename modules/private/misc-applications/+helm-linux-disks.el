;;; private/misc-applications/helm-linux-disks.el -*- lexical-binding: t; -*-

(use-package! helm-linux-disks
  :defer t
  :when (eq system-type 'gnu/linux)
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        "D" #'helm-linux-disks))
