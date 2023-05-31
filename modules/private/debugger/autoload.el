;;; private/debugger/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-debugger-quit-session-a ()
  (ignore-errors
    (let ((ignore-window-parameters t))
      (cl-loop for buf being the buffers
               when (string-match-p "gdb" (buffer-name buf)) do
               (cae-hacks-always-yes-a #'doom-kill-buffer-and-windows buf)))
    (hydra-keyboard-quit)))
