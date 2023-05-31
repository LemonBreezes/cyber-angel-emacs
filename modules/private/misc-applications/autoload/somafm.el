;;; private/misc-applications/autoload/somafm.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +somafm ()
  (interactive)
  (if (cae-display-graphic-p)
      (call-interactively #'somafm)
    (call-interactively #'somafm-by-completion)))
