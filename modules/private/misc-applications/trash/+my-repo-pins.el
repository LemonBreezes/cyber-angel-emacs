;;; private/misc-applications/trash/+my-repo-pins.el -*- lexical-binding: t; -*-

(use-package! my-repo-pins
  :defer t :init
  (map! :map +misc-applications-standalone-apps-map
        "j" #'my-repo-pins)
  :config
  (make-directory "~/code-root" t)
  (setq my-repo-pins-code-root "~/code-root"))
