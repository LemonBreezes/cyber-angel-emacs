;;; private/misc-applications/autoload/empv.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+empv-keymap "private/misc-applications/autoload/empv" nil t)
(defun +empv-keymap () (interactive))
(hercules-def
 :toggle-funs #'+empv-keymap
 :keymap 'empv-map
 :package 'empv)
