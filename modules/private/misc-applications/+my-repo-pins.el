;;; private/misc-applications/+my-repo-pins.el -*- lexical-binding: t; -*-

(use-package! my-repo-pins
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        "j" #'my-repo-pins)
  :config
  (make-directory "~/code-root" t)
  (setopt my-repo-pins-code-root "~/code-root"))
