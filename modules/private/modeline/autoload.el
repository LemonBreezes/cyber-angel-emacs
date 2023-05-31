;;; private/modeline/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(cae-defun cae-modeline-rotate-next-word-at-point ()
  (interactive)
  (skip-syntax-forward "^w")
    (when (eolp)
      (skip-syntax-backward "^w")
      (when-let ((beg (car-safe (bounds-of-thing-at-point 'word))))
        (goto-char beg)))
    (call-interactively #'parrot-rotate-next-word-at-point))
