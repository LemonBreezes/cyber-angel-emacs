;;; private/misc-applications/autoload/snow.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-snow (&optional arg)
  (interactive "P")
  (if arg
    (setq cae-snow--old-wconf nil)
  (if (modulep! :ui workspaces)
      (+workspace-switch cae-snow-workspace-name t)
    (setq cae-snow--old-wconf (current-window-configuration))
    (let ((ignore-window-parameters t))
      (delete-other-windows))
    (switch-to-buffer (doom-fallback-buffer))))
  (call-interactively #'snow)
  (with-current-buffer "*snow*"
    (local-set-key (kbd "q") #'cae-snow-quit)
    (when (featurep 'evil)
      (evil-local-set-key 'normal (kbd "q") #'cae-snow-quit))))

;;;###autoload
(defun cae-snow-quit ()
  (interactive)
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p cae-snow-workspace-name)
        (+workspace/kill cae-snow-workspace-name))
    (when cae-snow--old-wconf
      (set-window-configuration cae-snow--old-wconf)))
  (kill-buffer "*snow*"))
