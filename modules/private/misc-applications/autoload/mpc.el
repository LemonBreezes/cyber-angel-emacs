;;; private/misc-applications/autoload/mpc.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +mpc (&optional arg)
  (interactive "P")
  (if arg
      (setq +mpc--old-wconf nil)
    (if (modulep! :ui workspaces)
        ;; delete current workspace if empty
        ;; this is useful when mu4e is in the daemon
        ;; as otherwise you can accumulate empty workspaces
        (progn
          (unless (+workspace-buffer-list)
            (+workspace-delete (+workspace-current-name)))
          (+workspace-switch +mpc-workspace-name t))
      (setq +mpc--old-wconf (current-window-configuration))
      (delete-other-windows)
      (switch-to-buffer (doom-fallback-buffer))))
  (call-interactively #'mpc))


;;;###autoload
(defun +mpc-quit ()
  (interactive)
  (mpc-quit)
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p +mpc-workspace-name)
        (+workspace/delete +mpc-workspace-name))
    (when +mpc--old-wconf
      (set-window-configuration +mpc--old-wconf))))
