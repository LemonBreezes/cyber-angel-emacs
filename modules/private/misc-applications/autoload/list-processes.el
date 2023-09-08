;;; private/misc-applications/autoload/list-processes.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+list-processes-hydra/body "private/misc-applications/autoload/list-processes" nil t)
(eval `(defhydra +list-processes-hydra (:color pink :hint nil)
         ("<f6>" nil "Exit" :exit t)
         ("q" quit-window nil :exit t)
         ("S" tabulated-list-sort "Sort")
         ,@(if (modulep! :editor evil)
               '(("D" process-menu-delete-process "Delete process")
                 ("gr" revert-buffer "Refresh"))
             '(("d" process-menu-delete-process "Delete process")
               ("g" revert-buffer "Refresh"))))
      t)
