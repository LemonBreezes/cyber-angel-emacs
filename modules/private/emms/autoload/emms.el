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
  ["Quick Access"
   [("v" "VGM" (lambda () (interactive) (dired "/mnt/music/VGM/")))
    ("y" "Youtube Music" (lambda () (interactive) (dired "/mnt/music/Youtube Music/")))
    ("p" "Playlists" (lambda () (interactive) (dired "/mnt/music/Playlists/")))
    ("a" "Anime Music" (lambda () (interactive) (dired "/mnt/music/Anime Music/")))
    ("m" "EMMS indexed" (lambda () (interactive) (dired emms-source-file-default-directory)))]])

;; The following two functions are from
;; https://www.reddit.com/r/emacs/comments/qg2d0k/emms_modeline_shows_full_path_to_the_songs_i_only/

;;;###autoload
(defun cae-track-title-from-file-name (file)
  "For using with EMMS description functions. Extracts the track
title from the file name FILE, which just means a) taking only
the file component at the end of the path, and b) removing any
file extension."
  (with-temp-buffer
    (save-excursion (insert (file-name-nondirectory (directory-file-name file))))
    (ignore-error 'search-failed
      (search-forward-regexp (rx "." (+ alnum) eol))
      (delete-region (match-beginning 0) (match-end 0)))
    (buffer-string)))

;;;###autoload
(defun my-emms-track-description (track)
  "Return a description of TRACK, for EMMS, but try to cut just
the track name from the file name, and just use the file name too
rather than the whole path."
  (let ((artist (emms-track-get track 'info-artist))
        (title (emms-track-get track 'info-title)))
    (cond ((and artist title)
           ;; Converting the artist/title to a string works around a bug in `emms-info-exiftool'
           ;; where, if your track name is a number, e.g. "1999" by Jeroen Tel, then it will be an
           ;; integer type here, confusing everything.
           ;;
           ;; I would fix the bug properly and submit a patch but I just cannot be bothered to
           ;; figure out how to do that.
           (concat (format "%s" artist) " - " (format "%s" title)))
          (title title)
          ((eq (emms-track-type track) 'file)
           (track-title-from-file-name (emms-track-name track)))
          (t (emms-track-simple-description track)))))
