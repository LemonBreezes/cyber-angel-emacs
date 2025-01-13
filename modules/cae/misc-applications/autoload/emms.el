;;; private/emms/autoload/emms.el -*- lexical-binding: t; -*-

(require 'transient)

;;;###autoload
(defun cae-emms (&optional arg)
  (interactive "P")
  (unless (string-prefix-p "emms-" (symbol-name major-mode))
    (setq cae-emms--old-wconf (current-window-configuration))
    (let ((ignore-window-parameters t))
      (delete-other-windows)))
  (call-interactively #'emms-smart-browse))

;;;###autoload
(defun cae-emms-quit ()
  (interactive)
  (call-interactively #'emms-browser-bury-buffer)
  (when cae-emms--old-wconf
    (set-window-configuration cae-emms--old-wconf)))

(defun cae-emms-jump-to-currently-playing-track (&rest args)
  (let ((track (emms-track-get
                (emms-playlist-current-selected-track) 'name)))
    (if (and track args)
        (dired-jump nil track)
      (message "No song is currently selected.")
      (transient-setup 'cae-emms-quick-access))))

;; TODO PR something so that the callback always runs to EMMS even when tracks is nil.
(defadvice! cae-emms-handle-jump-to-playing-track-a (closure tracks)
  :after #'emms-player-mpd-sync-from-mpd-1
  (when (and (not tracks)
             (eq (cadr closure) #'cae-emms-jump-to-currently-playing-track))
    (funcall (cadr closure))))

;;;###autoload (autoload 'cae-emms-quick-access "cae/misc-applications/autoload/emms" nil t)
(transient-define-prefix cae-emms-quick-access ()
  "Jump to EMMS music directories."
  ["Quick Access"
   [("v" "VGM"
     (lambda () (interactive)
       (dired (expand-file-name "VGM" cae-misc-applications-music-dir))))
    ("y" "Youtube Music"
     (lambda () (interactive)
       (dired (expand-file-name "Youtube Music" cae-misc-applications-music-dir))))
    ("p" "Playlists"
     (lambda () (interactive)
       (dired (expand-file-name "Playlists" cae-misc-applications-music-dir))))
    ("a" "Anime Music"
     (lambda () (interactive)
       (dired (expand-file-name "Anime Music" cae-misc-applications-music-dir))))
    ("r" "Artists"
     (lambda () (interactive)
       (dired (expand-file-name "Artists" emms-source-file-default-directory))))
    ("l" "Longplays"
     (lambda () (interactive)
       (dired (expand-file-name "Longplays" emms-source-file-default-directory))))
    ("j" "Currently playing"
     (lambda () (interactive)
       (require 'emms)
       (if (and (executable-find "mpd")
                cae-misc-applications-mpd-host)
           (emms-player-mpd-sync-from-mpd nil #'cae-emms-jump-to-currently-playing-track)
         (funcall #'cae-emms-jump-to-currently-playing-track nil))))]])

;; The following two functions are from
;; https://www.reddit.com/r/emacs/comments/qg2d0k/emms_modeline_shows_full_path_to_the_songs_i_only/

(defun cae-emms-track-title-from-file-name (file)
  "Extract the track title from the file name FILE by taking only
the file component at the end of the path and removing any file extension."
  (file-name-sans-extension (file-name-nondirectory (directory-file-name file))))

;;;###autoload
(defun cae-emms-track-description (track)
  "Return a description of TRACK, for EMMS, but try to cut just
the track name from the file name, and just use the file name too
rather than the whole path."
  (let ((artist (emms-track-get track 'info-artist))
        (title (emms-track-get track 'info-title)))
    (cond ((and (stringp artist) (stringp title)
                (not (string-empty-p artist)) (not (string-empty-p title)))
           ;; Converting the artist/title to a string works around a bug in `emms-info-exiftool'
           ;; where, if your track name is a number, e.g. "1999" by Jeroen Tel, then it will be an
           ;; integer type here, confusing everything.
           ;;
           ;; I would fix the bug properly and submit a patch but I just cannot be bothered to
           ;; figure out how to do that.
           (if (<= (frame-width) 120)
               (format "%s" title)
             (concat (format "%s" artist) " - " (format "%s" title))))
          ((and (stringp title) (not (string-empty-p title)))
           title)
          ((eq (emms-track-type track) 'file)
           (cae-emms-track-title-from-file-name (emms-track-name track)))
          (t (emms-track-simple-description track)))))

(defvar cae-dired-emms-mode-map (make-sparse-keymap)
  "Keymap for `cae-dired-emms-mode'.")

(add-hook 'cae-dired-emms-mode-hook #'dired-hide-details-mode)

;;;###autoload
(define-minor-mode cae-dired-emms-mode
  "Minor mode to set up EMMS key bindings in Dired."
  :lighter " Dired-EMMS"
  :keymap cae-dired-emms-mode-map)

(map! :map cae-dired-emms-mode-map
      :gn "a" `(menu-item "" nil
                :filter ,(lambda (&optional _)
                           (when buffer-read-only
                             #'cae-emms-quick-access)))
      :gn "e" `(menu-item "" nil
                :filter ,(lambda (&optional _)
                           (when buffer-read-only
                             (require 'emms)
                             #'emms-play-dired)))
      :gn "E" `(menu-item "" nil
                :filter ,(lambda (&optional _)
                           (when buffer-read-only
                             (require 'emms)
                             (emms-stop)
                             (with-current-emms-playlist
                               (emms-playlist-clear)
                               (call-interactively #'emms-add-dired)
                               (call-interactively #'emms-playlist-shuffle)
                               (emms-playlist-select 1)
                               (emms-start))
                             ;; TODO Improve this so that `describe-key' gives
                             ;; useful information.
                             #'ignore)))
      :gn "." `(menu-item "" nil
                :filter ,(lambda (&optional _)
                           (when buffer-read-only
                             (require 'emms)
                             #'cae-emms-dired-hydra/body))))

;;;###autoload
(defun cae-emms-kill-mpv ()
  (interactive)
  (start-process "kill-mpv" nil "killall" "mpv"))

;;;###autoload (autoload 'cae-emms-dired-hydra/body "cae/misc-applications/autoload/emms" t nil)
(defhydra cae-emms-dired-hydra (:color pink :hint nil)
  ("." nil "Exit" :exit t)
  ("q" cae-emms-kill-mpv "Kill MPV" :exit t)
  ("j" emms-next "Next song" :column "Navigate")
  ("k" emms-previous "Previous song" :column "Navigate")
  ("<" emms-seek-backward "Seek backward" :column "Navigate")
  (">" emms-seek-forward "Seek forward" :column "Navigate")
  ("s" emms-shuffle "Shuffle" :column "Playback")
  ("x" emms-pause "Toggle play" :column "Playback"))

;;;###autoload
(defun cae-dired-emms-mode-hook-h ()
  (when (and (derived-mode-p 'dired-mode)
             (file-in-directory-p default-directory cae-misc-applications-music-dir))
    (cae-dired-emms-mode +1)))
