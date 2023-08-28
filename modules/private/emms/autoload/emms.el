;;; private/emms/autoload/emms.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +emms (&optional arg)
  (interactive "P")
  (if arg
      (setq +emms--old-wconf nil)
    (if (modulep! :ui workspaces)
        ;; delete current workspace if empty
        ;; this is useful when mu4e is in the daemon
        ;; as otherwise you can accumulate empty workspaces
        (progn
          (unless (+workspace-buffer-list)
            (+workspace-delete (+workspace-current-name)))
          (+workspace-switch +emms-workspace-name t))
      (setq +emms--old-wconf (current-window-configuration))
      (delete-other-windows)
      (switch-to-buffer (doom-fallback-buffer))))
  (call-interactively #'emms-smart-browse))


;;;###autoload
(defun +emms-quit ()
  (interactive)
  (call-interactively #'emms-browser-bury-buffer)
  (if (modulep! :ui workspaces)
      (when (+workspace-exists-p +emms-workspace-name)
        (+workspace/delete +emms-workspace-name))
    (when +emms--old-wconf
      (set-window-configuration +emms--old-wconf))))

;;;###autoload (autoload '+emms-quick-access "private/emms/autoload/emms" nil t)
(transient-define-prefix +emms-quick-access ()
  "Jump to EMMS music directories."
  ;; /mnt/music/VGM/
  ;; /mnt/Youtube Music/
  ;; /mnt/music/Playlists/
  ;; /mnt/music/Anime Music/
  ;; /mnt/music/Artists/
  ;; make a transient that will open the above directories in dired
  ["Quick Access"
   [("VGM" (lambda () (interactive) (dired "/mnt/music/VGM/")))
    ("Youtube Music" (lambda () (interactive) (dired "/mnt/Youtube Music/")))
    ("Playlists" (lambda () (interactive) (dired "/mnt/music/Playlists/")))
    ("Anime Music" (lambda () (interactive) (dired "/mnt/music/Anime Music/")))
    ("Artists" (lambda () (interactive) (dired "/mnt/music/Artists/")))]])
