;; -*- no-byte-compile: t; -*-
;;; cae/holy/packages.el

(package! cae-lib :recipe (:host github :repo "LemonBreezes/cae-lib"))

(package! anzu)
(package! goggles)
(package! edit-indirect)
(package! string-edit-at-point)
(package! symbol-overlay)
(package! speedrect :recipe (:host github :repo "jdtsmith/speedrect"))
(package! tabgo)
(when (modulep! :editor multiple-cursors)
  (package! mc-extras))
(package! expand-region-improved :recipe (:host github :repo "leotaku/expand-region-improved"))

;; These are used for their autoloads. They are not explicitly referenced in
;; this configuration.
(package! ialign)
