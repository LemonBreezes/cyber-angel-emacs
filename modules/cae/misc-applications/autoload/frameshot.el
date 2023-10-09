;;; private/misc-applications/autoload/frameshot.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +frameshot-take ()
  (interactive)
  (frameshot-clear)
  (frameshot-take))
