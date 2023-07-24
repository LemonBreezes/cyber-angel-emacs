;;; private/misc-applications/+bubbles.el -*- lexical-binding: t; -*-

(use-package! bubbles
  :defer t
  :init
  (map! :map +misc-applications-games-map
        "b" #'bubbles)
  :config
  (map! :map bubbles-mode-map
        :n "RET" #'bubbles-plop
        "<f6>" #'+bubbles-hydra/body
        "ta" #'bubbles-set-graphics-theme-ascii
        "tb" #'bubbles-set-graphics-theme-balls
        "te" #'bubbles-set-graphics-theme-emacs
        "tc" #'bubbles-set-graphics-theme-circles
        "ts" #'bubbles-set-graphics-theme-squares
        "td" #'bubbles-set-graphics-theme-diamonds
        "dh" #'bubbles-set-game-hard
        "de" #'bubbles-set-game-easy
        "dm" #'bubbles-set-game-medium
        "dd" #'bubbles-set-game-difficult
        "du" #'bubbles-set-game-userdefined
        "S" #'bubbles-save-settings))
