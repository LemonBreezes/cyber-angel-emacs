;;; private/misc-applications/autoload/timer-list.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+timer-list-hydra/body "private/misc-applications/autoload/timer-list" nil t)
(defhydra +timer-list-hydra (:color pink :hint nil)
  ("<f6>" nil "Exit" :exit t)
  ("q" quit-window nil :exit t)
  ("c" timer-list-cancel "Cancel timer")
  ("S" tabulated-list-sort "Sort")
  ("g" revert-buffer "Refresh"))
