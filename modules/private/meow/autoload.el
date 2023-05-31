;;; private/meow/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-meow-use-keyboard-layout-a ()
  (setq last-command-event
        (cae-keyboard-remap-reverse last-command-event)))

;;;###autoload
(defun cae-meow-update-cursor-a (&rest _)
  (when-let ((cursor
              (and meow--current-state
                   (symbol-value
                    (intern (concat "meow-cursor-type-"
                                    (symbol-name meow--current-state)))))))
    (when (car-safe cursor)
      (setq cursor (car cursor)))
    (setq cursor-type cursor)
    (etcc--evil-set-cursor)))

;;;###autoload
(defun cae-meow-save-line ()
  "Fallback command for `+meow-save'."
  (interactive)
  (let ((beg (if (eobp)
                 (line-beginning-position 0)
               (line-beginning-position)))
        (end (line-beginning-position 2)))
    (kill-ring-save beg end)))
