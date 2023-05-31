;;; autoload/cae-keyboard.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-keyboard-insert-current-prefix (arg)
  "Insert the current prefix argument."
  (interactive "P")
  (insert (format "%s" arg)))
