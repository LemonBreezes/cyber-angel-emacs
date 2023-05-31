;;; lisp/cae-lib.el -*- lexical-binding: t; -*-

(defmacro cae-defun (name arglist &optional docstring &rest body)
  "`defun' but guaranteed return the created function."
  (declare (doc-string 3) (indent 2))
  `(progn (defun ,name ,arglist ,docstring ,@body)
          #',name))

(defun cae-ignore-errors-a (fun &rest args)
  "Ignore errors in FUN with ARGS."
  (ignore-errors (apply fun args)))

(defun cae-display-graphic-p ()
  (and (display-graphic-p)
       (not (daemonp))))

(defvar cae-tab-bar-before-switch-hook nil
  "Hook run before switching tabs.")
(defvar cae-tab-bar-after-switch-hook nil
  "Hook run after switching tabs.")

(advice-add #'tab-bar-select-tab
            :around
            (cae-defun cae-tab-bar-switch-run-hooks-a (fun &rest args)
              (run-hooks 'cae-tab-bar-before-switch-hook)
              (apply fun args)
              (run-hooks 'cae-tab-bar-after-switch-hook)))
