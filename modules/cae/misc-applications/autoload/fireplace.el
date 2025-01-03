;;; private/misc-applications/autoload/fireplace.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-fireplace (&optional arg)
  (interactive "P")
  (if arg
      (setq cae-fireplace--old-wconf nil)
    (if (modulep! :ui workspaces)
        (+workspace-switch cae-fireplace-workspace-name t)
      (setq cae-fireplace--old-wconf (current-window-configuration))
      (let ((ignore-window-parameters t))
        (delete-other-windows))
      (switch-to-buffer (doom-fallback-buffer))))
  (call-interactively #'fireplace))


;;;###autoload
(defun cae-fireplace-quit ()
  (interactive)
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p cae-fireplace-workspace-name)
        (+workspace/kill cae-fireplace-workspace-name))
    (when cae-fireplace--old-wconf
      (set-window-configuration cae-fireplace--old-wconf)))
  (kill-buffer fireplace-buffer-name))
