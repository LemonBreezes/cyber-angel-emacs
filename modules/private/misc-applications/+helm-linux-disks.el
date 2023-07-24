;;; private/misc-applications/helm-linux-disks.el -*- lexical-binding: t; -*-

(use-package! helm-linux-disks
  :defer t
  :when (and (eq system-type 'gnu/linux)
             (or (modulep! :private helm)
                 (modulep! :completion helm)))
  :init
  (map! :map +misc-applications-system-map
        :desc "disks" "D" #'helm-linux-disks)
  (which-key-add-keymap-based-replacements +misc-applications-system-map
    "D" "disks"))
