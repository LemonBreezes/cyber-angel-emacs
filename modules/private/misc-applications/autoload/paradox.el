;;; private/misc-applications/autoload/paradox.el -*- lexical-binding: t; -*-

(defun cae-paradox-menu-quick-help ()
  (interactive)
  (if (and (memq last-command '(cae-paradox-menu-quick-help
                                paradox-menu-quick-help)))
      (progn
        (+log (minibuffer-message))
        (clear-minibuffer-message))
    (call-interactively #'paradox-menu-quick-help)))
