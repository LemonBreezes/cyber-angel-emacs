;;; private/misc-applications/autoload/list-processes.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+list-processes-hydra/body "private/misc-applications/autoload/list-processes" nil t)
(defhydra +list-processes-hydra (:color pink :hint nil)
  ("<f6>" nil "Exit" :exit t)
  ("q" quit-window nil :exit t)
  ("d" process-menu-delete-process "Delete process")
  ;;("S" tabulated-list-sort "Sort")
  ("g" revert-buffer "Refresh"))
