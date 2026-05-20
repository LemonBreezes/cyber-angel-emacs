;; -*- no-byte-compile: t; -*-
;;; cae/ghostel/packages.el

(package! ghostel)
(when (modulep! :editor evil +everywhere)
  (package! evil-collection :recipe
    (:host github :repo "LemonBreezes/evil-collection" :branch "add-ghostel-module")))
