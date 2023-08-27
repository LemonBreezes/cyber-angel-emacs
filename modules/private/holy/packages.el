;; -*- no-byte-compile: t; -*-
;;; private/holy/packages.el

(package! anzu)
(package! goggles)
(package! isearch-mb)
(package! edit-indirect)
(package! string-edit-at-point)
(package! symbol-overlay)
(package! speedrect :recipe (:host github :repo "jdtsmith/speedrect"))
(when (modulep! :editor multiple-cursors)
  (package! mc-extras))
(package! expand-region-improved :recipe (:host github :repo "leotaku/expand-region-improved"))

;; These are used for their autoloads. They are not explicitly referenced in
;; this configuration.
(package! ialign)
