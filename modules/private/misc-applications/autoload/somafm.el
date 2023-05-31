;;; private/misc-applications/autoload/somafm.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +somafm ()
  (interactive)
  (if (display-graphic-p)
      (call-interactively #'somafm)
    (call-interactively #'somafm-by-completion)))
