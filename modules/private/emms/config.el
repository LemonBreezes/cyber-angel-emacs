;;; private/emms/config.el -*- lexical-binding: t; -*-

(defvar +emms-music-dir "/mnt/hdd/music/"
  "The directory where your music library is located.")
(add-to-list 'safe-local-variable-directories +emms-music-dir)

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
        :ng "q" #'+emms-quit
        :ng "a" #'+emms-quick-access)
  (emms-all)
  (emms-default-players)
  (setq emms-repeat-playlist t
        emms-repeat-track t
        emms-random-playlist t
        emms-later-do-interval 0.5
        emms-later-do-batch 10
        emms-source-file-directory-tree-function #'emms-source-file-directory-tree-find
        emms-source-file-default-directory (expand-file-name "Artists" +emms-music-dir)
        emms-player-mpd-music-directory (expand-file-name "Artists" +emms-music-dir)
        emms-info-native--max-num-vorbis-comments 48000
        emms-browser-covers #'emms-browser-cache-thumbnail-async
        emms-info-functions '(emms-info-exiftool)
        emms-browser-switch-to-playlist-on-add t)
  (map! :map emms-playlist-mode-map
        :localleader
        "l" #'emms-toggle-repeat-playlist
        "p" #'emms-insert-playlist
        "i" #'emms-insert-file
        "t" #'emms-toggle-repeat-track
        "s" #'emms-playlist-save
        "m" #'emms-shuffle)
  (add-to-list 'emms-track-initialize-functions 'emms-info-initialize-track)
  (add-hook 'emms-browser-mode-hook #'doom-mark-buffer-as-real-h)
  (add-hook 'emms-playlist-mode-hook #'doom-mark-buffer-as-real-h)

  (setq emms-track-description-function 'cae-emms-track-description))

(use-package! helm-emms
  :when (or (modulep! :private helm)
            (modulep! :completion helm))
  :defer t :init
  (map! :leader
        :desc "Helm EMMS" "eh" #'helm-emms)
  :config
  (setq helm-emms-dired-directories (list (expand-file-name +emms-music-dir))
        helm-emms-use-track-description-function t
        helm-emms-directory-files-recursive-fn #'helm-emms-walk-directory-with-find
        helm-emms-default-sources '(helm-source-emms-files
                                    helm-source-emms-streams
                                    helm-source-emms-dired)))
