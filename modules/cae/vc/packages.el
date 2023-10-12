;; -*- no-byte-compile: t; -*-
;;; private/vc/packages.el

(package! git-link)
(package! gitignore-templates)
(package! git-email :recipe (:host github :repo "LemonBreezes/git-email"))
(package! vc-backup :recipe (:repo "LemonBreezes/vc-backup"))
(package! magit-stats)
(package! embark-vc)
(package! igist) ; TODO
(package! blamer)
(when (modulep! :completion vertico)
  (package! consult-gh :recipe (:host github :repo "armindarvish/consult-gh" :branch "main")))
(when (or (modulep! :completion helm)
          (modulep! :cae helm)))
(package! magit-stgit)
(package! stgit)
