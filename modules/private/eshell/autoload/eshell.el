;;; private/eshell/autoload/eshell.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +eshell-input-filter (str)
  "Filter some trivial commands from the input history."
  (not (or (string-blank-p str)
           (equal "cd" str)
           (string-prefix-p "cd " str)
           (string-prefix-p " " str))))

;;;###autoload
(defun +eshell-set-up-autocompletion ()
  (add-hook 'completion-at-point-functions
            #'cape-file nil t))
