;;; autoload/cae-bindings.el -*- lexical-binding: t; -*-

(defun cae-embark-act ()
  (interactive)
  (let ((embark-cycle-key (this-command-keys-vector)))
    (call-interactively 'embark-act)))
