;;; private/misc-applications/picpocket.el -*- lexical-binding: t; -*-

;; This package is pretty cool for viewing pictures. I might pick it up again in
;; the future. But for now, I'm okay with using Dirvish for viewing pictures.

(use-package! picpocket
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        "P" #'picpocket))
