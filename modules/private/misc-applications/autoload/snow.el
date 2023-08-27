;;; private/misc-applications/autoload/snow.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +snow ()
  (interactive)
  (if (modulep! :ui workspaces)
      ;; delete current workspace if empty
      ;; this is useful when mu4e is in the daemon
      ;; as otherwise you can accumulate empty workspaces
      (progn
        (unless (+workspace-buffer-list)
          (+workspace-delete (+workspace-current-name)))
        (+workspace-switch +snow-workspace-name t))
    (setq +snow--old-wconf (current-window-configuration))
    (delete-other-windows)
    (switch-to-buffer (doom-fallback-buffer)))
  (call-interactively #'snow)
  (local-set-key (kbd "q") #'+snow-quit)
  (when (featurep 'evil)
    (evil-local-set-key 'normal (kbd "q") #'+snow-quit)))


;;;###autoload
(defun +snow-quit ()
  (interactive)
  (if (modulep! :ui workspaces)
      (+workspace/delete +snow-workspace-name)
    (set-window-configuration +snow--old-wconf))
  (kill-buffer "*snow*"))
