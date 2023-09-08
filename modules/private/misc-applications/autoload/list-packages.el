;;; private/misc-applications/autoload/list-packages.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+list-packages-hydra/body "private/misc-applications/autoload/list-packages" nil t)
(eval ;; TODO
 `(defhydra +list-packages-hydra (:color pink :hint nil)
    ("<f6>" nil "Exit" :exit t)
    ("q" quit-window nil :exit t)
    ("S" tabulated-list-sort "Sort"))
      t)
