;; -*- no-byte-compile: t; -*-
;;; private/vc/packages.el

(package! git-link)
(package! gitignore-templates)
(package! git-email :recipe (:repo "git@git.sr.ht:~lemon_breezes/git-email"))
(package! vc-backup :recipe (:repo "LemonBreezes/vc-backup"))

;; This package should be installed by Doom, but it's not.  I don't know why.
(package! git-timemachine)
