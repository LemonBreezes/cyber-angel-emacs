;;; private/misc-applications/autoload/tetris.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+tetris-hydra/body "private/misc-applications/autoload/tetris" nil t)
(defhydra +tetris-hydra (:color pink :hint nil)
  ("<f6>" nil "Exit" :exit t)
  ("q" tetris-end-game nil :exit t)
  ("n" tetris-start-game "Restart game" :column "Misc")
  ("p" tetris-pause-game "Pause/Unpause game" :column "Misc")
  ("SPC" tetris-move-bottom "Move bottom" :column "Movement")
  ("<up>" tetris-rotate-prev "Rotate clockwise" :column "Movement")
  ("<left>" tetris-move-left "Move left" :column "Movement")
  ("<right>" tetris-move-right "Move right" :column "Movement")
  ("a" autotetris-mode "Autotetris" :column "Misc"))

;;;###autoload
(defun +tetris (&optional arg)
  (interactive "P")
  (if arg
      (setq +tetris--old-wconf nil)
    (if (modulep! :ui workspaces)
        ;; delete current workspace if empty
        ;; this is useful when mu4e is in the daemon
        ;; as otherwise you can accumulate empty workspaces
        (progn
          (unless (+workspace-buffer-list)
            (+workspace-delete (+workspace-current-name)))
          (+workspace-switch +tetris-workspace-name t))
      (setq +tetris--old-wconf (current-window-configuration))
      (delete-other-windows)
      (switch-to-buffer (doom-fallback-buffer))))
  (let* ((saves-buf (find-file-noselect (expand-file-name "tetris-scores" shared-game-score-directory)))
         (highest-score (with-current-buffer saves-buf
                          (local-set-key (kbd "q") #'+tetris-quit)
                          (when (featurep 'evil)
                            (evil-local-set-key 'normal (kbd "q") #'+tetris-quit))
                          (buffer-substring-no-properties (goto-char (point-min))
                                                          (pos-eol)))))
    (call-interactively #'tetris)
    (setq-local header-line-format highest-score)))

;;;###autoload
(defun +tetris-quit ()
  (interactive)
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p +tetris-workspace-name)
        (+workspace/delete +tetris-workspace-name))
    (when +tetris--old-wconf
      (set-window-configuration +tetris--old-wconf)))
  (kill-buffer "*Tetris*")
  (when-let ((saves-buf (get-file-buffer (expand-file-name "tetris-scores" shared-game-score-directory))))
    (with-current-buffer saves-buf
      (save-buffer)
      (kill-buffer))))
