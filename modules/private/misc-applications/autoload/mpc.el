;;; private/misc-applications/autoload/mpc.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+mpc-hydra/body "private/misc-applications/autoload/mpc" nil t)
(eval
 `(defhydra +mpc-hydra (:color pink :hint nil)
    ("<f6>" nil "Exit" :exit t)
    ("q" +mpc-quit nil :exit t)
    ("<" mpc-prev "Previous song" :column "Navigate")
    (">" mpc-next "Next song" :column "Navigate")
    (,(if (modulep! :editor evil) "t" "s") mpc-toggle-play "Toggle play" :column "Toggle")
    ,@(when (modulep! :editor evil)
        '(("r" mpc-toggle-repeat "Toggle repeat" :column "Toggle")
          ("s" mpc-toggle-shuffle "Toggle shuffle" :column "Toggle")
          ("c" mpc-toggle-consume "Toggle consume" :column "Toggle")
          ("p" mpc-playlist "Show playlist" :column "Playlist")
          ("a" mpc-playlist-add "Add to playlist" :column "Playlist")
          ("x" mpc-play-at-point "Play at point" :column "Playlist")))
    ("p" mpc-pause "Pause" :column "Toggle")
    ("g" mpc-seek-current "Seek current" :column "Navigate")
    ("o" mpc-goto-playing-song "Goto playing song" :column "Movement"))
 t)


(defvar +mpc--wconf nil)

;;;###autoload
(defun +mpc (&optional arg)
  (interactive "P")
  (setq +mpc--old-wconf (current-window-configuration))
  (delete-other-windows)
  (if +mpc--wconf
      (set-window-configuration +mpc--wconf)
    (call-interactively #'mpc))
  (setq +mpc--wconf (current-window-configuration)))

;;;###autoload
(defun +mpc-quit ()
  (interactive)
  (when +mpc--old-wconf
    (set-window-configuration +mpc--old-wconf)
    (setq +mpc--old-wconf nil)))
