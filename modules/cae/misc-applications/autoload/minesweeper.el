;;; cae/misc-applications/autoload/minesweeper.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-minesweeper (&optional arg)
  (interactive "P")
  (if arg
      (setq cae-minesweeper--old-wconf nil)
    (if (modulep! :ui workspaces)
        (progn (+workspace-switch cae-minesweeper-workspace-name t)
               (set-persp-parameter 'dont-save-to-file t
                                    (+workspace-get cae-minesweeper-workspace-name)))
      (setq cae-minesweeper--old-wconf (current-window-configuration))
      (let ((ignore-window-parameters t))
        (delete-other-windows))
      (switch-to-buffer (doom-fallback-buffer))))
  (call-interactively #'minesweeper)
  (dolist (win (window-list))
    (if (parent-mode-is-derived-p (buffer-local-value 'major-mode (window-buffer win))
                                  'minesweeper-mode)
        (when (modulep! :ui workspaces)
          (persp-add-buffer (window-buffer win)))
      (delete-window win))))

;;;###autoload
(defun cae-minesweeper-quit ()
  (interactive)
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p cae-minesweeper-workspace-name)
        (+workspace/kill cae-minesweeper-workspace-name))
    (when cae-minesweeper--old-wconf
      (set-window-configuration cae-minesweeper--old-wconf)))
  (kill-buffer "*minesweeper*"))
