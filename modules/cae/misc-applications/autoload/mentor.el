;;; private/misc-applications/autoload/mentor.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-mentor ()
  (interactive)
  (when (modulep! :ui workspaces)
    (+workspace-switch cae-mentor-workspace-name t)
    (+workspace/display))
  (call-interactively #'mentor))

;;;###autoload
(defun cae-mentor-quit ()
  (interactive)
  (bury-buffer)
  (if (and (modulep! :ui workspaces)
           (+workspace-exists-p cae-mentor-workspace-name))
      (progn (+workspace-kill cae-mentor-workspace-name)
             (+workspace/other))
    (bury-buffer)))
