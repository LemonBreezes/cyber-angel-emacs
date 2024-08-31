;; -*- no-byte-compile: t; -*-
;;; private/vc/packages.el

(package! git-link)
(package! gitignore-templates)
(package! vc-backup)
(package! embark-vc)
(when (modulep! :completion vertico)
  (package! consult-gh :recipe (:host github :repo "armindarvish/consult-gh" :branch "main"
                                :files "*")))
(when (or (modulep! :completion helm)
          (modulep! :cae helm)))
(when (modulep! +delta)
  (package! diff-ansi))
(package! difftastic)
(package! magit-file-icons)

;; These are used for their autoloads. They are not explicitly referenced in
;; this configuration.
(package! magit-stats)
