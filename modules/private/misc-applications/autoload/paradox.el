;;; private/misc-applications/autoload/paradox.el -*- lexical-binding: t; -*-

(defun cae-paradox-menu-quick-help ()
  (interactive)
  (if (and ;;(memq last-command '(cae-paradox-menu-quick-help
           ;;                     paradox-menu-quick-help))
           (current-message))
      (progn ;;(clear-minibuffer-message)
             (message "HELLOOOO"))
    (call-interactively #'paradox-menu-quick-help)))
