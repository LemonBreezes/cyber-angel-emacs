;;; private/misc-applications/speed-type.el -*- lexical-binding: t; -*-

(use-package! speed-type
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-games-prefix
        "T" #'speed-type-text)
  :config
  (map! :map speed-type--completed-keymap
        "q" #'kill-this-buffer
        "r" #'speed-type--replay
        "n" #'speed-type--play-next))
