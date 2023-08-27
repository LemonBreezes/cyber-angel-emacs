;;; private/misc-applications/autoload/fireplace.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+fireplace-hydra/body "private/misc-applications/autoload/fireplace" nil t)
(defhydra +fireplace-hydra (:color pink :hint nil)
  ("<f6>" nil "Exit" :exit t)
  ("q" fireplace-off nil :exit t)
  ("C-*" fireplace-toggle-smoke "Toggle smoke")
  ("C-+" fireplace-up "Fireplace up")
  ("C--" fireplace-down "Fireplace down")
  ("C-=" fireplace-toggle-sound "Toggle sound"))

;;;###autoload
(defun +fireplace ()
  (interactive)
  (if (modulep! :ui workspaces)
      ;; delete current workspace if empty
      ;; this is useful when mu4e is in the daemon
      ;; as otherwise you can accumulate empty workspaces
      (progn
        (unless (+workspace-buffer-list)
          (+workspace-delete (+workspace-current-name)))
        (+workspace-switch +fireplace-workspace-name t))
    (setq +fireplace--old-wconf (current-window-configuration))
    (delete-other-windows)
    (switch-to-buffer (doom-fallback-buffer)))
  (call-interactively #'fireplace))


;;;###autoload
(defun +fireplace-quit ()
  (interactive)
  (if (modulep! :ui workspaces)
      (+workspace/delete +fireplace-workspace-name)
    (set-window-configuration +fireplace--old-wconf))
  (kill-buffer fireplace-buffer-name))
