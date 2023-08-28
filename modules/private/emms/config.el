;;; private/emms/config.el -*- lexical-binding: t; -*-

(use-package! emms
  :defer t :init
  (setq emms-directory (concat doom-data-dir "emms")
        emms-cache-file (concat doom-cache-dir "emms"))
  :config
  (emms-all)
  (emms-default-players)
  (setq emms-repeat-playlist t
        emms-later-do-interval 0.01
        emms-later-do-batch 20
        emms-source-file-directory-tree-function #'emms-source-file-directory-tree-find
        emms-source-file-default-directory (expand-file-name "/mnt/unindexed-music/")
        emms-player-mpd-music-directory (expand-file-name "/mnt/unindexed-music/")
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
        "m" #'emms-shuffle))

(use-package! helm-emms
  :defer t :init
  (map! :map +misc-applications-music-map
        "e" #'helm-emms)
  :config
  (setq helm-emms-dired-directories (list (expand-file-name "/mnt/unindexed-music/"))
        helm-emms-use-track-description-function t
        helm-emms-directory-files-recursive-fn #'helm-emms-walk-directory-with-find
        helm-emms-default-sources '(;;helm-source-emms-files
                                    helm-source-emms-streams
                                    helm-source-emms-dired)))
