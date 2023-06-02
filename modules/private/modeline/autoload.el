;;; private/modeline/autoload.el -*- lexical-binding: t; -*-

(defun cae-modeline--rotate-word-at-point (rotate-function)
  (save-excursion
    (when-let ((beg (car-safe (bounds-of-thing-at-point 'symbol))))
      (goto-char beg))
    (skip-syntax-forward "^w" (line-end-position))
    (condition-case (call-interactively rotate-function)
        (error
         (skip-syntax-backward "^w" (line-beginning-position))
         (call-interactively rotate-function)))))

;;;###autoload
(defun cae-modeline-rotate-forward-word-at-point ()
  (interactive)
  (cae-modeline--rotate-word-at-point #'parrot-rotate-next-word-at-point))

;;;###autoload
(defun cae-modeline-rotate-backward-word-at-point ()
  (interactive)
  (cae-modeline--rotate-word-at-point #'parrot-rotate-prev-word-at-point))
