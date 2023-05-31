;;; private/misc-applications/autoload/speed-type.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +evil-insert-a (&rest _)
  (when (modulep! :editor evil)
    (evil-insert-state)))

;;;###autoload (autoload '+speed-type-hydra/body "private/misc-applications/autoload/speed-type" nil t)
(defhydra +speed-type-hydra (:color blue :hint nil)
  ("<f6>" nil "Exit" :exit t)
  ("C-c C-k" speed-type-complete "Complete" :exit t))
