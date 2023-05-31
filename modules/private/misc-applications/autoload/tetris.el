;;; private/misc-applications/autoload/tetris.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+tetris-hydra/body "private/misc-applications/autoload/tetris" nil t)
(defhydra +tetris-hydra (:color pink :hint nil)
  ("<f6>" nil "Exit" :exit t)
  ("q" tetris-end-game nil :exit t)
  ("n" tetris-start-game "Start game")
  ("p" tetris-pause-game "Pause game")
  ("SPC" tetris-move-bottom "Move bottom")
  ("<up>" tetris-rotate-prev "Rotate clockwise")
  ("<left>" tetris-move-left "Move left" :column "Movement")
  ("<right>" tetris-move-right "Move right" :column "Movement"))
