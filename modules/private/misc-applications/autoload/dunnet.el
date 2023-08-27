;;; private/misc-applications/autoload/dunnet.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +dunnet (&optional arg)
  (interactive "P")
  (if arg
      (setq +dunnet--old-wconf nil)
    (if (modulep! :ui workspaces)
        ;; delete current workspace if empty
        ;; this is useful when mu4e is in the daemon
        ;; as otherwise you can accumulate empty workspaces
        (progn
          (unless (+workspace-buffer-list)
            (+workspace-delete (+workspace-current-name)))
          (+workspace-switch +dunnet-workspace-name t))
      (setq +dunnet--old-wconf (current-window-configuration))
      (delete-other-windows)
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
  (kill-buffer "*dunnet*"))
