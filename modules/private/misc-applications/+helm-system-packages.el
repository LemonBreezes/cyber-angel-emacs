;;; private/misc-applications/helm-system-packages.el -*- lexical-binding: t; -*-

(use-package! helm-system-packages
  :defer t
  :when (and (not (memq system-type '(cygwin windows-nt ms-dos)))
             (or (modulep! :private helm)
                 (modulep! :completion helm)))
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        "P" #'helm-system-packages))
