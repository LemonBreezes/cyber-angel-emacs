;;; autoload/cae-exwm.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-open-eshell-in-new-workspace ()
  "Open a new eshell in a new workspace."
  (interactive)
  (+workspace/new "Eshell")
  (eshell))
