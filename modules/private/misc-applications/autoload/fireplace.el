;;; private/misc-applications/autoload/fireplace.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+fireplace-hydra/body "private/misc-applications/autoload/fireplace" nil t)
(defhydra +fireplace-hydra (:color pink :hint nil)
  ("<f6>" nil "Exit" :exit t)
  ("C-*" fireplace-toggle-smoke "Toggle smoke")
  ("C-+" fireplace-up "Fireplace up")
  ("C--" fireplace-down "Fireplace down")
  ("C-=" fireplace-toggle-sound "Toggle sound"))
