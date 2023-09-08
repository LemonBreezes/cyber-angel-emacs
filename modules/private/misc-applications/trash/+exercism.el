;;; private/misc-applications/+exercism.el -*- lexical-binding: t; -*-

;; I never really got this package working to be honest...

(use-package! exercism-modern
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        (:prefix ("E" . "exercism")
         "v" #'exercism-modern-view-tracks
         "j" #'exercism-modern-jump)))
