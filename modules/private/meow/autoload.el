;;; private/meow/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-meow-use-keyboard-layout-a ()
  (setq last-command-event
        (cae-keyboard-remap-reverse last-command-event)))

;;;###autoload
(defun cae-meow-update-cursor-a ()
  (when-let ((cursor
              (symbol-value
               (intern (concat "meow-cursor-type-"
                               (symbol-name meow--current-state))))))
    (when (car-safe cursor)
      (setq cursor (car cursor)))
    (setq cursor-type cursor)
    (etcc--evil-set-cursor)))
