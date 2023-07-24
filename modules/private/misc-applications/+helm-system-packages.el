;;; private/misc-applications/helm-system-packages.el -*- lexical-binding: t; -*-

(use-package! helm-system-packages
  :defer t
  :when (and (not (memq system-type '(cygwin windows-nt ms-dos)))
             (or (modulep! :private helm)
                 (modulep! :completion helm)))
  :init
  (map! :map +misc-applications-system-map
        "s" #'helm-system-packages)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-system-map
      "s" "system packages")))
