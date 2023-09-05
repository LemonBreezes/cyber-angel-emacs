;;; private/misc-applications/autoload/dunnet.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +dunnet (&optional arg)
  (interactive "P")
  (if arg
      (setq +dunnet--old-wconf nil)
    (if (modulep! :ui workspaces)
        (+workspace-switch +dunnet-workspace-name t)
      (setq +dunnet--old-wconf (current-window-configuration))
      (let ((ignore-window-parameters t))
        (delete-other-windows))
      (switch-to-buffer (doom-fallback-buffer))))
  (call-interactively #'dunnet))

;;;###autoload
(defun +dunnet-quit ()
  (interactive)
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p +dunnet-workspace-name)
        (+workspace/delete +dunnet-workspace-name))
    (when +dunnet--old-wconf
      (set-window-configuration +dunnet--old-wconf)))
  (kill-buffer "*dungeon*"))
