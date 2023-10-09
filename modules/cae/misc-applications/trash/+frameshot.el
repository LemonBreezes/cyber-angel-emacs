;;; private/misc-applications/+frameshot.el -*- lexical-binding: t; -*-

;; I prefer to use Flameshot to take screenshots.

(use-package! frameshot
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        "F" #'+frameshot-take)
  :config
  (setq frameshot-config `((output . ,doom-picture-dir))))
