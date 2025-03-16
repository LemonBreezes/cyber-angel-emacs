;; -*- no-byte-compile: t; -*-
;;; cae/org/packages.el

(package! cae-lib :recipe (:host github :repo "LemonBreezes/cae-lib"))

(package! org-rich-yank)
(package! org-inline-pdf)
(package! language-detection)
(package! org-web-tools)
(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))
(package! org-view-mode)

(when (modulep! +pretty)
  (package! org-modern))
