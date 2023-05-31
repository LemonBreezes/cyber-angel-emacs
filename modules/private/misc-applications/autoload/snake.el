;;; private/misc-applications/autoload/snake.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+snake-hydra/body "private/misc-applications/autoload/snake" nil t)
(defhydra +snake-hydra (:color pink :hint nil)
  ("<f6>" nil "Exit" :exit t)
  ("q" snake-end-game nil :exit t)
  ("n" snake-start-game "Start game" :column "Misc")
  ("p" snake-pause-game "Pause/Unpause game" :column "Misc")
  ("<down>" snake-move-down "Move down" :column "Movement")
  ("<up>" snake-move-up "Move up" :column "Movement")
  ("<left>" snake-move-left "Move left" :column "Movement")
  ("<right>" snake-move-right "Move right" :column "Movement"))
