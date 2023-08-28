;;; private/emms/config.el -*- lexical-binding: t; -*-

(use-package! emms
  :defer t :init
  (setq emms-directory (concat doom-data-dir "emms")
        emms-cache-file (concat doom-cache-dir "emms"))
  :config
  (emms-all)
  (emms-default-players)
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
        helm-emms-default-sources '(helm-source-emms-files
                                    helm-source-emms-streams
                                    helm-source-emms-dired)))
