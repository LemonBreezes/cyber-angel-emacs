;;; private/misc-applications/speed-type.el -*- lexical-binding: t; -*-

(use-package! speed-type
  :defer t
  :init
  (map! :map +misc-applications-games-map
        "T" #'speed-type-text)
  :config
  (add-hook 'speed-type-mode-hook #'visual-line-mode)
  (when (modulep! :private corfu)
    (add-to-list 'corfu-excluded-modes #'speed-type-mode))
  (map! :map speed-type--completed-keymap
        "q" #'kill-this-buffer
        "r" #'speed-type--replay
        "n" #'speed-type--play-next
        :map speed-type-mode-map
        "<f6>" #'+speed-type-hydra/body))
