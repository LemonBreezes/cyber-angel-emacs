;;; private/modeline/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-modeline-rotate-forward-word-at-point ()
  (interactive)
  (save-excursion
    (when-let ((beg (car-safe (bounds-of-thing-at-point 'symbol))))
      (goto-char beg))
    (skip-syntax-forward "^w" (line-end-position))
    (condition-case (call-interactively #'parrot-rotate-next-word-at-point)
        (error
         (skip-syntax-backward "^w" (line-beginning-position))
         (call-interactively #'parrot-rotate-next-word-at-point)))))

;;;###autoload
(defun cae-modeline-rotate-backward-word-at-point ()
  (interactive)
  (save-excursion
    (when-let ((beg (car-safe (bounds-of-thing-at-point 'symbol))))
      (goto-char beg))
    (skip-syntax-forward "^w" (line-end-position))
    (condition-case (call-interactively #'parrot-rotate-prev-word-at-point)
        (error
         (skip-syntax-backward "^w" (line-beginning-position))
         (call-interactively #'parrot-rotate-prev-word-at-point)))))
