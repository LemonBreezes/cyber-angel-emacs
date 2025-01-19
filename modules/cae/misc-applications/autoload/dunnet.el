;;; private/misc-applications/autoload/dunnet.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-dunnet (&optional arg)
  (interactive "P")
  (if arg
      (setq cae-dunnet--old-wconf nil)
    (if (modulep! :ui workspaces)
        (progn (+workspace-switch cae-dunnet-workspace-name t)
               (set-persp-parameter 'dont-save-to-file t cae-dunnet-workspace-name))
      (setq cae-dunnet--old-wconf (current-window-configuration))
      (let ((ignore-window-parameters t))
        (delete-other-windows))
      (switch-to-buffer (doom-fallback-buffer))))
  (call-interactively #'dunnet)
  (when (modulep! :ui workspaces)
    (persp-add-buffer (current-buffer))))

;;;###autoload
(defun cae-dunnet-quit ()
  (interactive)
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p cae-dunnet-workspace-name)
        (+workspace/kill cae-dunnet-workspace-name))
    (when cae-dunnet--old-wconf
      (set-window-configuration cae-dunnet--old-wconf)))
  (kill-buffer "*dungeon*"))
