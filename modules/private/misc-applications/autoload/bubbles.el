;;; private/misc-applications/autoload/bubbles.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +bubbles (&optional arg)
  (interactive "P")
  (if arg
      (setq +bubbles--old-wconf nil)
    (if (modulep! :ui workspaces)
        (+workspace-switch +bubbles-workspace-name t)
      (setq +bubbles--old-wconf (current-window-configuration))
      (let ((ignore-window-parameters t))
        (delete-other-windows))
      (switch-to-buffer (doom-fallback-buffer))))
  (call-interactively #'bubbles))

;;;###autoload
(defun +bubbles-quit ()
  (interactive)
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p +bubbles-workspace-name)
        (+workspace/delete +bubbles-workspace-name))
    (when +bubbles--old-wconf
      (set-window-configuration +bubbles--old-wconf)))
  (kill-buffer "*bubbles*"))
