;;; private/modeline/autoload/minions.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-modeline-minions-c-setup ()
  (require 'glasses)
  (setq-local minions-available-modes
              (append '((hide-ifdef-mode)
                        (glasses-mode))
                      minions-available-modes)))

;;;###autoload
(defun cae-modeline-minions-elisp-setup ()
  (setq-local minions-available-modes
              (append '((nameless-mode))
                      minions-available-modes)))
