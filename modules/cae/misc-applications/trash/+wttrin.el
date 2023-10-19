;;; private/misc-applications/trash/+wttrin.el -*- lexical-binding: t; -*-

;; This package didn't really work that well but it's okay if you really need to
;; see how cold it is outside. Hopefully I will have a better alternative in
;; this module soon.

(use-package! wttrin
  :defer t :init
  (map! :map +misc-applications-external-apps-map
        "w" #'wttrin)
  (defadvice! +wttrin-setup-h (&rest _)
    :after #'wttrin-query
    (face-remap-add-relative 'default :family "Iosevka" :height 1.0)))
