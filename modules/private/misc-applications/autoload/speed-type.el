;;; private/misc-applications/autoload/speed-type.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +speed-type-text (&optional arg)
  (interactive "P")
  (if arg
      (setq +speed-type--old-wconf nil)
    (if (modulep! :ui workspaces)
        (+workspace-switch +speed-type-workspace-name t)
      (setq +speed-type--old-wconf (current-window-configuration))
      (let ((ignore-window-parameters t))
        (delete-other-windows))
      (switch-to-buffer (doom-fallback-buffer))))
  (call-interactively #'speed-type-text))

;;;###autoload
(defun +speed-type-quit ()
  (interactive)
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p +speed-type-workspace-name)
        (+workspace/delete +speed-type-workspace-name))
    (when +speed-type--old-wconf
      (set-window-configuration +speed-type--old-wconf)))
  (kill-buffer "speed-type"))
