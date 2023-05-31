;;; private/modeline/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-modeline-rotate-next-word-at-point ()
  (interactive)
  (save-excursion
    (skip-syntax-forward "^w" (line-end-position))
    (call-interactively #'parrot-rotate-next-word-at-point)))
