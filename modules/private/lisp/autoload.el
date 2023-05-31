;;; private/lisp/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-lisp-check-parens-before-save-h ()
  (add-hook 'write-file-functions 'check-parens nil t))

;;;###autoload
(defun cae-lisp-add-safe-local-variables-on-save ()
  "Add variables in .dir-locals.el as safe values when the file is saved."
  (when (and (buffer-file-name)
             (string= (file-name-nondirectory (buffer-file-name)) ".dir-locals.el"))
    (let ((locals (dir-locals-read-from-file (buffer-file-name))))
      (dolist (entry locals)
        (let ((mode (car entry))
              (variables (cdr entry)))
          (dolist (variable variables)
            (let ((var (car variable))
                  (value (cdr variable)))
              (put var 'safe-local-variable
                   (lambda (_x) (equal _x value))))))))))
