;;; private/misc-applications/+exercism.el -*- lexical-binding: t; -*-

(use-package! exercism-modern
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        (:prefix ("e" . "exercism")
                 "v" #'exercism-modern-view-tracks
                 "j" #'exercism-modern-jump)))
