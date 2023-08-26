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

(defun cae-which-key-inhibit-hook ()
  (setq which-key-inhibit nil)
  (remove-hook 'pre-command-hook
               #'cae-which-key-inhibit-hook))

(defun cae-which-key-show-map (keymap)
  (setq which-key-inhibit t)
  (add-hook 'pre-command-hook #'cae-which-key-inhibit-hook)
  (run-with-idle-timer
   which-key-idle-delay nil
   `(lambda ()
      (when which-key-inhibit
        (which-key-show-keymap
         ',keymap)))))

