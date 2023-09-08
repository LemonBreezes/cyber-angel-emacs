;;; private/misc-applications/vuiet.el -*- lexical t; -*-

;; I might pick this one back up again one day. My biggest complaint is the lack
;; of a track looping feature.

(use-package! vuiet
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        (:prefix ("v" . "vuiet")
         "l" #'vuiet-play-loved-track
         "w" #'vuiet-play-track-by-lyrics
         "a" #'vuiet-artist-info-search
         "s" #'vuiet-play-track-search
         "t" #'vuiet-play-tag-similar))
  :config
  (map! :leader
        :prefix +misc-applications-prefix
        (:prefix ("v" . "vuiet")
         "L" #'vuiet-loved-tracks-info
         "W" #'vuiet-playing-track-lyrics
         "b" #'vuiet-album-info-search
         "x" #'vuiet-stop
         "." #'vuiet-next
         "," #'vuiet-previous
         "+" #'vuiet-love-track
         "p" #'vuiet-play-pause
         "B" #'vuiet-info-playing-track-album
         "A" #'vuiet-playing-artist-info))
  (setq vuiet-automatic-lyrics t)
  (add-hook 'vuiet-mode-hook #'+vuiet-buffer-hook))
