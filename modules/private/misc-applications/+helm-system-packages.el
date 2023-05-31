;;; private/misc-applications/helm-system-packages.el -*- lexical-binding: t; -*-

(use-package! helm-system-packages
  :defer t
  :when (not (memq system-type '(cygwin windows-nt ms-dos)))
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        "p" #'helm-system-packages))
