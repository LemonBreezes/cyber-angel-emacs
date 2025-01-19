;;; cae/misc-applications/autoload/chess.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-chess (&optional arg)
  (interactive "P")
  (if arg
      (setq cae-chess--old-wconf nil)
    (if (modulep! :ui workspaces)
        (+workspace-switch cae-chess-workspace-name t)
      (setq cae-chess--old-wconf (current-window-configuration))
      (let ((ignore-window-parameters t))
        (delete-other-windows))
      (switch-to-buffer (doom-fallback-buffer))))
  (call-interactively #'chess)
  (dolist (win (window-list))
    (if (parent-mode-is-derived-p (buffer-local-value 'major-mode (window-buffer win))
                                  'chess-display-mode)
        (when (modulep! :ui workspaces)
          (persp-add-buffer (window-buffer win)))
      (delete-window win))))

;;;###autoload
(defun cae-chess-quit ()
  (interactive)
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p cae-chess-workspace-name)
        (+workspace/kill cae-chess-workspace-name))
    (when cae-chess--old-wconf
      (set-window-configuration cae-chess--old-wconf)))
  (kill-buffer "*chess*"))
