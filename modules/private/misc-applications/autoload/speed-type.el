;;; private/misc-applications/autoload/speed-type.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+speed-type-hydra/body "private/misc-applications/autoload/speed-type" nil t)
(defhydra +speed-type-hydra (:color blue :hint nil)
  ("<f6>" nil "Exit" :exit t)
  ("C-c C-k" speed-type-complete "Complete" :exit t))

;;;###autoload
(defun +speed-type-text (&optional arg)
  (interactive "P")
  (if arg
      (setq +speed-type--old-wconf nil)
    (if (modulep! :ui workspaces)
        (+workspace-switch +speed-type-workspace-name t)
      (setq +speed-type--old-wconf (current-window-configuration))
      (let ((ignore-window-parameters t))
        (delete-other-windows))
      (switch-to-buffer (doom-fallback-buffer))))
  (call-interactively #'speed-type-text))

;;;###autoload
(defun +speed-type-quit ()
  (interactive)
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p +speed-type-workspace-name)
        (+workspace/delete +speed-type-workspace-name))
    (when +speed-type--old-wconf
      (set-window-configuration +speed-type--old-wconf)))
  (kill-buffer "speed-type"))
