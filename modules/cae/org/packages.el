;; -*- no-byte-compile: t; -*-
;;; private/org/packages.el

(package! cae-lib :recipe (:host github :repo "LemonBreezes/cae-lib"))

(package! org-rich-yank)
(package! org-inline-pdf)
(package! language-detection)
(package! org-web-tools)

(when (modulep! +pretty)
  (package! org-modern))
