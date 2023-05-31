;;; private/lisp/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +check-parens-before-save-h ()
  (add-hook 'write-file-functions 'check-parens nil t))
