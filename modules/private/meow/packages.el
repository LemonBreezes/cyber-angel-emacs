;; -*- no-byte-compile: t; -*-
;;; editor/meow/packages.el

(package! meow)
(package! evil-terminal-cursor-changer :recipe (:host github :repo "7696122/evil-terminal-cursor-changer"))
(package! embrace :recipe (:type git :host github :repo "cute-jumper/embrace.el"
                           :fork (:host github :repo "alexluigit/embrace.el")))
