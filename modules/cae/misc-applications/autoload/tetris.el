;;; private/misc-applications/autoload/tetris.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-tetris (&optional arg)
  (interactive "P")
  (if arg
      (setq cae-tetris--old-wconf nil)
    (if (modulep! :ui workspaces)
        (progn (+workspace-switch cae-tetris-workspace-name t)
               (set-persp-parameter 'dont-save-to-file t cae-tetri-workspace-name))
      (setq cae-tetris--old-wconf (current-window-configuration))
      (delete-other-windows)
      (switch-to-buffer (doom-fallback-buffer))))
  (let* ((saves-buf (find-file-noselect (expand-file-name "tetris-scores" shared-game-score-directory)))
         (highest-score (with-current-buffer saves-buf
                          (local-set-key (kbd "q") #'cae-tetris-quit)
                          (when (featurep 'evil)
                            (evil-local-set-key 'normal (kbd "q") #'cae-tetris-quit))
                          (buffer-substring-no-properties (goto-char (point-min))
                                                          (pos-eol)))))
    (call-interactively #'tetris)
    (when (modulep! :ui workspaces)
      (persp-add-buffer (current-buffer)))
    (setq-local header-line-format highest-score)))

;;;###autoload
(defun cae-tetris-quit ()
  (interactive)
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p cae-tetris-workspace-name)
        (+workspace/kill cae-tetris-workspace-name))
    (when cae-tetris--old-wconf
      (set-window-configuration cae-tetris--old-wconf)))
  (kill-buffer "*Tetris*")
  (when-let* ((saves-buf (get-file-buffer (expand-file-name "tetris-scores" shared-game-score-directory))))
    (with-current-buffer saves-buf
      (save-buffer)
      (kill-buffer))))
