;;; private/misc-applications/autoload/forecast.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +forecast ()
  (interactive)
  (when (modulep! :ui workspaces)
    (+workspace-switch "*forecast*" t)
    (+workspace/display))
  (forecast))

;;;###autoload
(defun +forecast-quit ()
  (interactive)
  (if (and (modulep! :ui workspaces)
           (+workspace-exists-p "*forecast*"))
      (progn (+workspace-delete "*forecast*")
             (+workspace/other))
    (bury-buffer)))
