;; -*- no-byte-compile: t; -*-
;;; private/org/packages.el

(package! org-rich-yank)
(package! org-inline-pdf)
(package! language-detection)
(package! org-appear)

(when (modulep! +pretty)
  (package! org-tidy :recipe (:host github :repo "jxq0/org-tidy"))
  (package! org-modern))
