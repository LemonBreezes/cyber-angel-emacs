;;; private/misc-applications/autoload/mpc.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+mpc-hydra/body "private/misc-applications/autoload/mpc" nil t)
(eval
 `(defhydra +mpc-hydra (:color pink :hint nil)
    ("<f6>" nil "Exit" :exit t)
    ("q" +mpc-quit nil :exit t)
    ("<" mpc-prev "Previous song")
    (">" mpc-next "Next song")
    (,(if (modulep! :editor evil) "t" "s") mpc-toggle-play "Toggle play")
    ,@(when (modulep! :editor evil)
        '(("r" mpc-toggle-repeat "Toggle repeat")
          ("s" mpc-toggle-shuffle "Toggle shuffle")
          ("c" mpc-toggle-consume "Toggle consume")
          ("p" mpc-playlist "Show playlist")
          ("a" mpc-playlist-add "Add to playlist")
          ("x" mpc-play-at-point "Play at point")))
    ("p" mpc-pause "Pause")
    ("g" mpc-seek-current "Seek current")
    ("o" mpc-goto-playing-song "Goto playing song"))
 t)

;;;###autoload
(defun +mpc (&optional arg)
  (interactive "P")
  (if arg
      (setq +mpc--old-wconf nil)
    (if (modulep! :ui workspaces)
        ;; delete current workspace if empty
        ;; this is useful when mu4e is in the daemon
        ;; as otherwise you can accumulate empty workspaces
        (progn
          (unless (+workspace-buffer-list)
            (+workspace-delete (+workspace-current-name)))
          (+workspace-switch +mpc-workspace-name t))
      (setq +mpc--old-wconf (current-window-configuration))
      (delete-other-windows)
      (switch-to-buffer (doom-fallback-buffer))))
  (call-interactively #'mpc))


;;;###autoload
(defun +mpc-quit ()
  (interactive)
  (mpc-quit)
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p +mpc-workspace-name)
        (+workspace/delete +mpc-workspace-name))
    (when +mpc--old-wconf
      (set-window-configuration +mpc--old-wconf))))
