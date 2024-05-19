;;; autoload/cae-exwm.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-open-eshell-in-new-workspace ()
  "Open a new eshell in a new workspace."
  (interactive)
  (unless (+workspace-exists-p "Eshell")
    (+workspace/new "Eshell"))
  (eshell)
  (delete-other-windows))
