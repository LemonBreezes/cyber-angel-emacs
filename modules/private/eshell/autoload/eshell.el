;;; private/eshell/autoload/eshell.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-eshell-set-up-autocompletion ()
  (add-hook 'completion-at-point-functions
            #'cape-file nil t))
