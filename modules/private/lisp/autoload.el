;;; private/lisp/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-lisp-check-parens-before-save-h ()
  (add-hook 'write-file-functions 'check-parens))

;;;###autoload
(defun cae-lisp-eval-buffer-before-save-h ()
  (add-hook 'write-file-functions 'eval-buffer 1 t))
