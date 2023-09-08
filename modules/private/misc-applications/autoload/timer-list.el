;;; private/misc-applications/autoload/timer-list.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+timer-list-hydra/body "private/misc-applications/autoload/timer-list" nil t)
(eval `(defhydra +timer-list-hydra (:color pink :hint nil)
         ("<f6>" nil "Exit" :exit t)
         ("q" quit-window nil :exit t)
         ("S" tabulated-list-sort "Sort")
         ,@(if (modulep! :editor evil)
               '(("C" timer-list-cancel "Cancel timer")
                 ("gr" revert-buffer "Refresh"))
             '(("c" timer-list-cancel "Cancel timer")
              ("g" revert-buffer "Refresh"))))
      t)
