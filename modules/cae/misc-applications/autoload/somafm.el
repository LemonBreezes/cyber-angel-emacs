;;; private/misc-applications/autoload/somafm.el -*- lexical-binding: t; -*-

(eval-when-compile (require 'hydra nil t))

;;;###autoload
(defun +somafm ()
  (interactive)
  (require 'somafm)
  (if (cae-display-graphic-p)
      (call-interactively #'somafm)
    (call-interactively #'somafm-by-completion)))
