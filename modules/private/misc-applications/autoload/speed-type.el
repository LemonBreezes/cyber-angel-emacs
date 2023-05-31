;;; private/misc-applications/autoload/speed-type.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +evil-insert-a (&rest _)
  (when (modulep! :editor evil)
    (evil-insert-state)))
