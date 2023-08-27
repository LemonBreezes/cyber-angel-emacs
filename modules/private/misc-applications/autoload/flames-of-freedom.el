;;; private/misc-applications/autoload/flames-of-freedom.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +flames-of-freedom (&optional arg)
  (interactive "P")
  (if arg
    (setq +flames-of-freedom--old-wconf nil)
  (if (modulep! :ui workspaces)
      ;; delete current workspace if empty
      ;; this is useful when mu4e is in the daemon
      ;; as otherwise you can accumulate empty workspaces
      (progn
        (unless (+workspace-buffer-list)
          (+workspace-delete (+workspace-current-name)))
        (+workspace-switch +flames-of-freedom-workspace-name t))
    (setq +flames-of-freedom--old-wconf (current-window-configuration))
    (delete-other-windows)
    (switch-to-buffer (doom-fallback-buffer))))
  (call-interactively #'flames-of-freedom-default))

(defun +flames-of-freedom-quit ()
  (if (modulep! :ui workspaces)
    (when (+workspace-exists-p +flames-of-freedom-workspace-name)
      (+workspace/delete +flames-of-freedom-workspace-name))
  (when +flames-of-freedom--old-wconf
    (set-window-configuration +flames-of-freedom--old-wconf))))

(advice-add #'flames-of-freedom-default :after #'+flames-of-freedom-quit)
