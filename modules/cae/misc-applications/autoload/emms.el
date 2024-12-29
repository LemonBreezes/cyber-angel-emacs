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

;;;###autoload (autoload 'cae-emms-quick-access "cae/misc-applications/autoload/emms" nil t)
(transient-define-prefix cae-emms-quick-access ()
  "Jump to EMMS music directories."
  ["Quick Access"
   [("v" "VGM"
     (lambda () (interactive)
       (require 'emms)
       (dired (expand-file-name "VGM" cae-misc-applications-music-dir))))
    ("y" "Youtube Music"
     (lambda () (interactive)
       (require 'emms)
       (dired (expand-file-name "Youtube Music" cae-misc-applications-music-dir))))
    ("p" "Playlists"
     (lambda () (interactive)
       (require 'emms)
       (dired (expand-file-name "Playlists" cae-misc-applications-music-dir))))
    ("a" "Anime Music"
     (lambda () (interactive)
       (require 'emms)
       (dired (expand-file-name "Anime Music" cae-misc-applications-music-dir))))
    ("r" "Artists"
     (lambda () (interactive)
       (require 'emms)
       (dired (expand-file-name "Artists" emms-source-file-default-directory))))
    ("l" "Longplays"
     (lambda () (interactive)
       (require 'emms)
       (dired (expand-file-name "Longplays" emms-source-file-default-directory))))
    ("j" "Currently playing"
     (lambda () (interactive)
       (require 'emms)
       (if emms-player-mpd-current-status
           (emms-player-mpd-sync-from-mpd
            nil
            (lambda (&rest _)
              (dired-jump nil (emms-track-get
                               (emms-playlist-current-selected-track) 'name))))
         (dired (expand-file-name "VGM" cae-misc-applications-music-dir)))))]])

;; The following two functions are from
;; https://www.reddit.com/r/emacs/comments/qg2d0k/emms_modeline_shows_full_path_to_the_songs_i_only/

(defun cae-emms-track-title-from-file-name (file)
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
(defun cae-emms-track-description (track)
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
           (if (<= (frame-width) 120)
               (format "%s" title)
             (concat (format "%s" artist) " - " (format "%s" title))))
          (title title)
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
                             #'cae-emms-quick-access
                             'ignore)))
      :gn "e" `(menu-item "" nil
                :filter ,(lambda (&optional _)
                           (when buffer-read-only
                             (call-interactively #'emms-play-dired)
                             (call-interactively #'emms-shuffle)
                             'ignore))))
