;;; private/misc-applications/+wttrin.el -*- lexical-binding: t; -*-

(use-package! wttrin
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-external-apps-prefix
        "w" #'wttrin)
  :config
  (advice-add #'wttrin-query :after
              (cae-defun +wttrin-setup-h (&rest _)
                (face-remap-add-relative 'default :family "Iosevka" :height 1.0))))
