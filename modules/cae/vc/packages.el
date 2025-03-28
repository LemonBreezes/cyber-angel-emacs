;; -*- no-byte-compile: t; -*-
;;; cae/vc/packages.el

(package! cae-lib :recipe (:host github :repo "lemonbreezes/cae-lib"))

(package! git-link)
(package! gitignore-templates)
(package! vc-backup)
(package! embark-vc)
(when (modulep! :completion vertico)
  (package! consult-gh :recipe (:host github :repo "armindarvish/consult-gh"
                                :files ("*"))))
(when (or (modulep! :completion helm)
          (modulep! :cae helm)))
(when (modulep! +delta)
  (package! diff-ansi))
(package! difftastic)

;; These are used for their autoloads. They are not explicitly referenced in
;; this configuration.
(package! magit-stats)
