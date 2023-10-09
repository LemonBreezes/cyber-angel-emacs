;;; private/misc-applications/autoload/paradox.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-paradox-menu-quick-help ()
  (interactive)
  (if (and (eq last-command #'paradox-menu-quick-help))
      (progn (clear-minibuffer-message)
             (setq this-command #'clear-minibuffer-message))
    (call-interactively #'paradox-menu-quick-help)
    (setq this-command #'paradox-menu-quick-help)))
