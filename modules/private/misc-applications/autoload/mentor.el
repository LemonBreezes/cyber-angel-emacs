;;; private/misc-applications/autoload/mentor.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +mentor ()
  (interactive)
  (when (modulep! :ui workspaces)
    (+workspace-switch +mentor-workspace-name t)
    (+workspace/display))
  (call-interactively #'mentor))

;;;###autoload
(defun +mentor-quit ()
  (interactive)
  (bury-buffer)
  (if (and (modulep! :ui workspaces)
           (+workspace-exists-p +mentor-workspace-name))
      (progn (+workspace-delete +mentor-workspace-name)
             (+workspace/other))
    (bury-buffer)))
