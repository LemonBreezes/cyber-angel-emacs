;;; private/misc-applications/autoload/fireplace.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +fireplace (&optional arg)
  (interactive "P")
  (if arg
      (setq +fireplace--old-wconf nil)
    (if (modulep! :ui workspaces)
        (+workspace-switch +fireplace-workspace-name t)
      (setq +fireplace--old-wconf (current-window-configuration))
      (let ((ignore-window-parameters t))
        (delete-other-windows))
      (switch-to-buffer (doom-fallback-buffer))))
  (call-interactively #'fireplace))


;;;###autoload
(defun +fireplace-quit ()
  (interactive)
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p +fireplace-workspace-name)
        (+workspace/kill +fireplace-workspace-name))
    (when +fireplace--old-wconf
      (set-window-configuration +fireplace--old-wconf)))
  (kill-buffer fireplace-buffer-name))
