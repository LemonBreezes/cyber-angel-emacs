;; -*- no-byte-compile: t; -*-
;;; private/org/packages.el

(package! org-rich-yank)
(package! org-inline-pdf)
(package! language-detection)
(package! org-web-tools)

(when (modulep! +pretty)
  (package! org-modern))
