;; -*- no-byte-compile: t; -*-
;;; cae/ghostel/packages.el

(package! ghostel)
(when (modulep! :editor evil +everywhere)
  (package! evil-collection :recipe
    ;; Added 05/20/26. Check back later to possibly PR.
    (:host github :repo "LemonBreezes/evil-collection" :branch "add-ghostel-module")))
