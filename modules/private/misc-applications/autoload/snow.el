;;; private/misc-applications/autoload/snow.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +snow (&optional arg)
  (interactive "P")
  (if arg
    (setq +snow--old-wconf nil)
  (if (modulep! :ui workspaces)
      (+workspace-switch +snow-workspace-name t)
    (setq +snow--old-wconf (current-window-configuration))
    (let ((ignore-window-parameters t))
      (delete-other-windows))
    (switch-to-buffer (doom-fallback-buffer))))
  (call-interactively #'snow)
  (with-current-buffer "*snow*"
    (local-set-key (kbd "q") #'+snow-quit)
    (when (featurep 'evil)
      (evil-local-set-key 'normal (kbd "q") #'+snow-quit))))

;;;###autoload
(defun +snow-quit ()
  (interactive)
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p +snow-workspace-name)
        (+workspace/delete +snow-workspace-name))
    (when +snow--old-wconf
      (set-window-configuration +snow--old-wconf)))
  (kill-buffer "*snow*"))
