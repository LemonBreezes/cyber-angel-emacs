;;; private/misc-applications/autoload/speed-type.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-speed-type-text (&optional arg)
  (interactive "P")
  (if arg
      (setq cae-speed-type--old-wconf nil)
    (if (modulep! :ui workspaces)
        (+workspace-switch cae-speed-type-workspace-name t)
      (setq cae-speed-type--old-wconf (current-window-configuration))
      (let ((ignore-window-parameters t))
        (delete-other-windows))
      (switch-to-buffer (doom-fallback-buffer))))
  (call-interactively #'speed-type-text))

;;;###autoload
(defun cae-speed-type-quit ()
  (interactive)
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p cae-speed-type-workspace-name)
        (+workspace/kill cae-speed-type-workspace-name))
    (when cae-speed-type--old-wconf
      (set-window-configuration cae-speed-type--old-wconf)))
  (kill-buffer "speed-type"))
