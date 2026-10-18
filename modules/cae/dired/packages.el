;; -*- no-byte-compile: t; -*-
;;; cae/dired/packages.el

(package! cae-lib :recipe (:host github :repo "LemonBreezes/cae-lib"))
(package! dired-rsync)

;; Transparent archive browsing. Needs the `avfsd' daemon from `avfs'.
(when (executable-find "avfsd")
  (package! dired-avfs))
