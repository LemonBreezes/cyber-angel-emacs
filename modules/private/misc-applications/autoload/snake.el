;;; private/misc-applications/autoload/snake.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+snake-hydra/body "private/misc-applications/autoload/snake" nil t)
(defhydra +snake-hydra (:color pink :hint nil)
  ("<f6>" nil "Exit" :exit t)
  ("q" snake-end-game nil :exit t)
  ("n" snake-start-game "Restart game" :column "Misc")
  ("p" snake-pause-game "Pause/Unpause game" :column "Misc")
  ("<down>" snake-move-down "Move down" :column "Movement")
  ("<up>" snake-move-up "Move up" :column "Movement")
  ("<left>" snake-move-left "Move left" :column "Movement")
  ("<right>" snake-move-right "Move right" :column "Movement"))

;;;###autoload
(defun +snake (&optional arg)
  (interactive "P")
  (if arg
      (setq +snake--old-wconf nil)
    (if (modulep! :ui workspaces)
        (+workspace-switch +snake-workspace-name t)
      (setq +snake--old-wconf (current-window-configuration))
      (let ((ignore-window-parameters t))
        (delete-other-windows))
      (switch-to-buffer (doom-fallback-buffer))))
  (let* ((saves-buf (find-file-noselect (expand-file-name "snake-scores" shared-game-score-directory)))
         (highest-score (with-current-buffer saves-buf
                          (local-set-key (kbd "q") #'+snake-quit)
                          (when (featurep 'evil)
                            (evil-local-set-key 'normal (kbd "q") #'+snake-quit))
                          (buffer-substring-no-properties (goto-char (point-min))
                                                          (pos-eol)))))
    (call-interactively #'snake)
    (setq-local header-line-format highest-score)))

;;;###autoload
(defun +snake-quit ()
  (interactive)
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p +snake-workspace-name)
        (+workspace/delete +snake-workspace-name))
    (when +snake--old-wconf
      (set-window-configuration +snake--old-wconf)))
  (kill-buffer "*Snake*")
  (when-let ((saves-buf (get-file-buffer (expand-file-name "snake-scores" shared-game-score-directory))))
    (with-current-buffer saves-buf
      (save-buffer)
      (kill-buffer))))
