;;; private/dired/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-dired-load-dirvish-h (dir)
  (remove-hook 'find-directory-functions #'cae-dired-load-dirvish-h)
  (require 'dirvish nil t)
  (unless (memq #'dired-noselect find-directory-functions)
    (add-hook 'find-directory-functions #'dired-noselect t))
  (dired-noselect dir))
