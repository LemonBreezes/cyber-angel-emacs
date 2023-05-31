;;; private/misc-applications/aurel.el -*- lexical-binding: t; -*-

(use-package! aurel
  :defer t
  :when (and (eq system-type 'gnu/linux) (executable-find "pacman"))
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        (:prefix-map ("A" . "AUR")
         "d" #'aurel-package-info
         "s" #'aurel-package-search
         "m" #'aurel-maintainer-search
         "i" #'aurel-installed-packages)))
