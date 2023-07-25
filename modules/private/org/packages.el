;; -*- no-byte-compile: t; -*-
;;; private/org/packages.el

(package! org-rich-yank)
(package! org-inline-pdf)
(package! language-detection)
(package! org-appear)
(package! org-jira)
(package! ejira)
(package! worf :recipe
  (:host github :repo "LemonBreezes/worf"))
(package! org-tidy)
(when (modulep! :tools lsp)
  (package! org-src-context :recipe (:host github :repo "karthink/org-src-context")))
