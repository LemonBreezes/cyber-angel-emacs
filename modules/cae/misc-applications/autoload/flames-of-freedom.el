;;; private/misc-applications/autoload/flames-of-freedom.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-flames-of-freedom (&optional arg)
  (interactive "P")
  (if arg
      (setq cae-flames-of-freedom--old-wconf nil)
    (if (modulep! :ui workspaces)
        (+workspace-switch cae-flames-of-freedom-workspace-name t)
      (setq cae-flames-of-freedom--old-wconf (current-window-configuration))
      (let ((ignore-window-parameters t))
        (delete-other-windows))
      (switch-to-buffer (doom-fallback-buffer))))
  (call-interactively #'flames-of-freedom-default)
  (when (modulep! :ui workspaces)
    (persp-add-buffer (current-buffer))))

(defun cae-flames-of-freedom-quit ()
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p cae-flames-of-freedom-workspace-name)
        (+workspace/kill cae-flames-of-freedom-workspace-name))
    (when cae-flames-of-freedom--old-wconf
      (set-window-configuration cae-flames-of-freedom--old-wconf))))

(advice-add #'flames-of-freedom-default :after #'cae-flames-of-freedom-quit)
