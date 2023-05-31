;;; private/meow/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-meow-use-keyboard-layout-a ()
  (setq last-command-event
        (cae-keyboard-remap-reverse last-command-event)))

;;;###autoload
(defun meow-localleader ()
  "Enter Doom's localleader keymap."
  (interactive)
  (meow--execute-kbd-macro doom-localleader-alt-key))
