;;; private/misc-applications/trash/+wttrin.el -*- lexical-binding: t; -*-

(use-package! wttrin
  :defer t :init
  (map! :map +misc-applications-external-apps-map
        "w" #'wttrin)
  (advice-add #'wttrin-query :after
              (cae-defun +wttrin-setup-h (&rest _)
                (face-remap-add-relative 'default :family "Iosevka" :height 1.0))))
