;;; private/misc-applications/autoload/dunnet.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-dunnet (&optional arg)
  (interactive "P")
  (if arg
      (setq cae-dunnet--old-wconf nil)
    (if (modulep! :ui workspaces)
        (+workspace-switch cae-dunnet-workspace-name t)
      (setq cae-dunnet--old-wconf (current-window-configuration))
      (let ((ignore-window-parameters t))
        (delete-other-windows))
      (switch-to-buffer (doom-fallback-buffer))))
  (call-interactively #'dunnet))

;;;###autoload
(defun cae-dunnet-quit ()
  (interactive)
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p cae-dunnet-workspace-name)
        (+workspace/kill cae-dunnet-workspace-name))
    (when cae-dunnet--old-wconf
      (set-window-configuration cae-dunnet--old-wconf)))
  (kill-buffer "*dungeon*"))
