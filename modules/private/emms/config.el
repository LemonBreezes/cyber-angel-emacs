;;; private/emms/config.el -*- lexical-binding: t; -*-

(use-package! emms
  :defer t :init
  (defvar +emms-workspace-name "*emms*")
  (defvar +emms--old-wconf nil)
  (setq emms-directory (concat doom-data-dir "emms/")
        emms-cache-file (concat emms-directory "cache"))
  (after! dired
    (map! :map dired-mode-map
          :ng "E" #'emms-play-dired))
  (map! :leader
        :prefix-map ("e" . "EMMS")
        :desc "Smart browse" "e" #'+emms)
  :config
  (map! :map emms-browser-mode-map
        :ng "q" #'+emms-quit)
  (emms-all)
  (emms-default-players)
  (setq emms-repeat-playlist t
        emms-later-do-interval 0.5
        emms-later-do-batch 1000
        emms-source-file-directory-tree-function #'emms-source-file-directory-tree-find
        emms-source-file-default-directory (expand-file-name "/mnt/music/")
        emms-player-mpd-music-directory (expand-file-name "/mnt/music/")
        emms-info-native--max-num-vorbis-comments 48000
        emms-browser-covers #'emms-browser-cache-thumbnail-async
        emms-info-functions '(emms-info-exiftool))
  (map! :map emms-playlist-mode-map
        :localleader
        "l" #'emms-toggle-repeat-playlist
        "p" #'emms-insert-playlist
        "i" #'emms-insert-file
        "t" #'emms-toggle-repeat-track
        "s" #'emms-playlist-save
        "m" #'emms-shuffle)
  (add-to-list 'emms-track-initialize-functions 'emms-info-initialize-track))

(use-package! helm-emms
  :when (modulep! :private helm)
  :defer t :init
  (map! :leader
        :desc "Helm EMMS" "eh" #'helm-emms)
  :config
  (setq helm-emms-dired-directories (list (expand-file-name "/mnt/music/"))
        helm-emms-use-track-description-function t
        helm-emms-directory-files-recursive-fn #'helm-emms-walk-directory-with-find
        helm-emms-default-sources '(helm-source-emms-files
                                    helm-source-emms-streams
                                    helm-source-emms-dired)))

(use-package! emms-mode-line-cycle
  :after emms
  :config
  (custom-set-variables
   '(emms-mode-line-cycle-max-width 25)
   '(emms-mode-line-cycle-additional-space-num 4)
   '(emms-mode-line-cycle-use-icon-p t)
   '(emms-mode-line-format " [%s]")
   '(emms-mode-line-cycle-any-width-p t)
   '(emms-mode-line-cycle-velocity 2)
   '(emms-mode-line-cycle-current-title-function
     (lambda ()
       (let ((track (emms-playlist-current-selected-track)))
         (cl-case (emms-track-type track)
           ((streamlist)
            (let ((stream-name (emms-stream-name
                                (emms-track-get track 'metadata))))
              (if stream-name stream-name (emms-track-description track))))
           ((url) (emms-track-description track))
           (t (file-name-nondirectory
               (emms-track-description track)))))))
   '(emms-mode-line-titlebar-function
     (lambda ()
       '(:eval
         (when emms-player-playing-p
           (format " %s %s"
                   (format emms-mode-line-format (emms-mode-line-cycle-get-title))
                   emms-playing-time-string))))))
  ;; Disable variable pitch fonts in the modeline because they look bad with the
  ;; cycling effect.
  (after! modus-themes
    (setq! modus-themes-variable-pitch-ui nil))
  (after! ef-themes
    (setq! ef-themes-variable-pitch-ui nil))
  (setq! emms-mode-line-icon-image-cache
         '(image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };")))
