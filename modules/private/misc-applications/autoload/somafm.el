;;; private/misc-applications/autoload/somafm.el -*- lexical-binding: t; -*-

;;;###autoload
(defun somafm-mode-line-indicator ()
  "Return a formatted string of the current song."
  (when somafm-current-song
    (list (somafm--format-current-song somafm-current-song) " ")))
