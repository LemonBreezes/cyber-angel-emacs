;;; private/misc-applications/autoload/paradox.el -*- lexical-binding: t; -*-

(defun cae-paradox-menu-quick-help ()
  (interactive)
  (if (memq 'last-command '(cae-paradox-menu-quick-help
                            paradox-menu-quick-help))
      (keyboard-quit)
    (call-interactively #'paradox-menu-quick-help)))
