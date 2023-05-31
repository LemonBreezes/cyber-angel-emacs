;;; private/meow/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-meow-use-keyboard-layout-a ()
  (+log last-command-event)
  (setq last-command-event
        (cae-keyboard-remap-char last-command-event)))
