;;; private/misc-applications/speed-type.el -*- lexical-binding: t; -*-

(use-package! speed-type
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-games-prefix
        "T" #'speed-type-text)
  :config
  (add-hook 'speed-type-mode-hook #'visual-line-mode)
  (when (modulep! :private corfu)
    (add-to-list 'corfu-excluded-modes #'speed-type-mode))
  (map! :map speed-type--completed-keymap
        "q" #'kill-this-buffer
        "r" #'speed-type--replay
        "n" #'speed-type--play-next))
