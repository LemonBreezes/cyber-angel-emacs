;;; lisp/cae-lib.el -*- lexical-binding: t; -*-

(defmacro cae-defun (name arglist &optional docstring &rest body)
  "`defun' but guaranteed return the created function."
  (declare (doc-string 3) (indent 2))
  `(progn (defun ,name ,arglist ,docstring ,@body)
          #',name))
