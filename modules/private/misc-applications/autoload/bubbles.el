;;; private/misc-applications/autoload/bubbles.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+bubbles-hydra/body "private/misc-applications/autoload/bubbles" nil t)
(defhydra +bubbles-hydra (:color pink :hint nil)
  ("<f6>" nil "Exit" :exit t)
  ("q" quit-window nil :exit t)
  ("f" forward-char "Move right" :column "Movement")
  ("b" backward-char "Move left" :column "Movement")
  ("n" next-line "Move down" :column "Movement")
  ("p" previous-line "Move up" :column "Movement"))
