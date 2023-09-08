;;; private/misc-applications/trash/+my-repo-pins.el -*- lexical-binding: t; -*-

;; Now I use `consult-gh' instead of this. They're not equivalent but the lack
;; of a search feature makes this package less useful.

(use-package! my-repo-pins
  :defer t :init
  (map! :map +misc-applications-standalone-apps-map
        "j" #'my-repo-pins)
  :config
  (make-directory "~/code-root" t)
  (setq my-repo-pins-code-root "~/code-root"))
