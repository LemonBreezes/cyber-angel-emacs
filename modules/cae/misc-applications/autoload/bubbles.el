;;; private/misc-applications/autoload/bubbles.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-bubbles (&optional arg)
  (interactive "P")
  (if arg
      (setq cae-bubbles--old-wconf nil)
    (if (modulep! :ui workspaces)
        (+workspace-switch cae-bubbles-workspace-name t)
      (setq cae-bubbles--old-wconf (current-window-configuration))
      (let ((ignore-window-parameters t))
        (delete-other-windows))
      (switch-to-buffer (doom-fallback-buffer))))
  (call-interactively #'bubbles))

;;;###autoload
(defun cae-bubbles-quit ()
  (interactive)
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p cae-bubbles-workspace-name)
        (+workspace/kill cae-bubbles-workspace-name))
    (when cae-bubbles--old-wconf
      (set-window-configuration cae-bubbles--old-wconf)))
  (kill-buffer "*bubbles*"))
