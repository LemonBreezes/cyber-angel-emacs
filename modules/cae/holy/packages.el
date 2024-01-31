;; -*- no-byte-compile: t; -*-
;;; private/holy/packages.el

(package! anzu)
(package! goggles)
(package! edit-indirect)
(package! string-edit-at-point)
(package! symbol-overlay)
(package! speedrect :recipe (:host github :repo "jdtsmith/speedrect"))
(package! tabgo)
(when (modulep! :editor multiple-cursors)
  (package! mc-extras))

;; These are used for their autoloads. They are not explicitly referenced in
;; this configuration.
(package! ialign)
