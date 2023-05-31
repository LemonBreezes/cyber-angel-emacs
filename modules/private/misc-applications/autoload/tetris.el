;;; private/misc-applications/autoload/tetris.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+tetris-hydra/body "private/misc-applications/autoload/tetris" nil t)
(defhydra +tetris-hydra (:color pink :hint nil)
  ("<f6>" nil "Exit" :exit t)
  ("q" tetris-end-game nil :exit t)
  ("n" tetris-start-game "Restart game" :column "Misc")
  ("p" tetris-pause-game "Pause/Unpause game" :column "Misc")
  ("SPC" tetris-move-bottom "Move bottom" :column "Movement")
  ("<up>" tetris-rotate-prev "Rotate clockwise" :column "Movement")
  ("<left>" tetris-move-left "Move left" :column "Movement")
  ("<right>" tetris-move-right "Move right" :column "Movement")
  ("C-a" autotetris-mode "Autotetris" :column "Misc"))
