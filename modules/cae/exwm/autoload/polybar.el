;;; cae/exwm/autoload/polybar.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-exwm-restart-polybar ()
  (interactive)
  (start-process "restart polybar" nil "polybar-msg" "cmd" "restart"))
