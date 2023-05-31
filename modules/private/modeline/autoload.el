;;; private/modeline/autoload.el -*- lexical-binding: t; -*-

(cae-defun cae-modeline-rotate-next-word-at-point ()
  (interactive)
  (save-excursion
    (skip-syntax-forward "^w")
    (call-interactively #'parrot-rotate-next-word-at-point)))
