;;; autoload/cae-bindings.el -*- lexical-binding: t; -*-

(defun cae-embark-act ()
  (interactive)
  (let ((embark-cycle-key (key-description (this-command-keys))))
    (call-interactively 'embark-act)))
