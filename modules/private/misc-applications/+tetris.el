;;; private/misc-applications/+tetris.el -*- lexical-binding: t; -*-

(use-package! tetris
  :defer t
  :init
  (map! (:map +misc-applications-games-map
         "t" #'tetris)
        (:map +misc-applications-eyecandy-map
         "t" #'autotetris))
  :config
  (map! :map tetris-mode-map
        "<f6>" #'+tetris-hydra/body
        "a" #'autotetris-mode)
  (map! :map autotetris-mode-map
        "a" nil))                       ;Not sure what `autotetris-move' even
                                        ;does to be honest.
