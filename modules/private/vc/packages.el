;; -*- no-byte-compile: t; -*-
;;; private/vc/packages.el

(package! git-link)
(package! gitignore-templates)
(package! git-email :recipe (:repo "git@git.sr.ht:~lemon_breezes/git-email"))
(package! git-email :recipe (:local-repo "packages/git-email"))
(package! vc-backup :recipe (:repo "LemonBreezes/vc-backup"))
(package! magit-stats)
(package! embark-vc)
