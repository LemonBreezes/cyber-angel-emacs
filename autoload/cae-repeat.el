;;; autoload/cae-repeat.el -*- lexical-binding: t; -*-

;;;###autoload
(defun other-window-previous (count &optional all-frames)
  "Select the previous window."
  (interactive "p\nP")
  (other-window (- count) all-frames t))
