;; -*- no-byte-compile: t; -*-
;;; private/org/packages.el

(package! org-rich-yank)
(package! org-inline-pdf)
(package! language-detection)
(package! org-appear)
(when (modulep! +jira)
  (package! org-jira))
(package! worf :recipe
  (:host github :repo "LemonBreezes/worf"))
(package! org-tidy)
