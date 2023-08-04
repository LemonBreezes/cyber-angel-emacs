;;; autoload/cae-exwm.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-exwm-start-dictation ()
  (interactive)
  (call-interactively #'exwm-edit--compose)
  (run-at-time exwm-edit-yank-delay nil
               (lambda () (call-interactively #'whisper-run))))
