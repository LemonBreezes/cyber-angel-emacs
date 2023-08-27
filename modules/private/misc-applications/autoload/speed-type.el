;;; private/misc-applications/autoload/speed-type.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+speed-type-hydra/body "private/misc-applications/autoload/speed-type" nil t)
(defhydra +speed-type-hydra (:color blue :hint nil)
  ("<f6>" nil "Exit" :exit t)
  ("C-c C-k" speed-type-complete "Complete" :exit t))

;;;###autoload
(defun +speed-type (&optional arg)
  (interactive "P")
  (if arg
      (setq +speed-type--old-wconf nil)
    (if (modulep! :ui workspaces)
        ;; delete current workspace if empty
        ;; this is useful when mu4e is in the daemon
        ;; as otherwise you can accumulate empty workspaces
        (progn
          (unless (+workspace-buffer-list)
            (+workspace-delete (+workspace-current-name)))
          (+workspace-switch +speed-type-workspace-name t))
      (setq +speed-type--old-wconf (current-window-configuration))
      (delete-other-windows)
      (switch-to-buffer (doom-fallback-buffer))))
  (call-interactively #'speed-type))

;;;###autoload
(defun +speed-type-quit ()
  (interactive)
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p +speed-type-workspace-name)
        (+workspace/delete +speed-type-workspace-name))
    (when +speed-type--old-wconf
      (set-window-configuration +speed-type--old-wconf)))
  (kill-buffer "*dungeon*"))

;;;###autoload
(defun +evil-insert-a (&rest _)
  (when (modulep! :editor evil)
    (evil-insert-state)))
