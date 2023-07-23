;;; private/misc-applications/autoload/paradox.el -*- lexical-binding: t; -*-

(defun cae-paradox-menu-quick-help ()
  (interactive)
  ;;(+log (current-message))
  (call-interactively #'paradox-menu-quick-help))
