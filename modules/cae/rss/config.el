;;; cae/rss/config.el -*- lexical-binding: t; -*-

(use-package! elfeed
  :defer t :init
  (map! :leader
        :desc "RSS" "ou" #'=rss)
  :config
  (setq elfeed-search-title-max-width 100)
  (defun cae-elfeed-set-filter (tag period)
    (lambda ()
      (interactive)
      (elfeed-search-set-filter (concat "@" period "-ago" (if tag (concat " +" tag) "")))))
  (let ((custom-filters
         `(("R" ,(cae-elfeed-set-filter "reddit" "6-months") "Reddit feeds" :column "Custom filters")
           ("E" ,(cae-elfeed-set-filter "emacs" "6-months") "Emacs feeds" :column "Custom filters")
           ("Y" ,(cae-elfeed-set-filter "tube" "6-months") "Youtube feeds" :column "Custom filters")
           ("*" ,(cae-elfeed-set-filter "star" "6-months") "Starred feeds" :column "Custom filters")
           ("a" ,(cae-elfeed-set-filter "" "6-months") "All feeds" :column "Custom filters")
           ("T" ,(cae-elfeed-set-filter "" "1-day") "Today's feeds" :column "Custom filters"))))
    (cl-loop for (key filter desc . rest) in custom-filters do
             (after! elfeed
               (map! :map elfeed-search-mode-map
                     :desc desc :ng key filter))))
  (after! recentf
    (push elfeed-db-directory recentf-exclude))
  (map! :map elfeed-show-mode-map
        :ng "o" #'link-hint-open-link
        :map elfeed-search-mode-map
        [remap elfeed-search-quit-window] #'+elfeed-quit
        :n "b" #'elfeed-search-browse-url
        :n "m" #'elfeed-toggle-star
        :n "F" #'elfeed-tube-fetch
        :n "l" #'+elfeed-toggle-log-buffer
        :ng "t" #'mark-whole-buffer)

  (use-package! elfeed-tube
    :config
    (elfeed-tube-setup)
    (map! :map elfeed-show-mode-map
          "F" #'elfeed-tube-fetch
          [remap save-buffer] #'elfeed-tube-save
          :localleader
          :desc "Youtube fetch" "f" #'elfeed-tube-fetch
          :map elfeed-search-mode-map
          "F" #'elfeed-tube-fetch
          [remap save-buffer] #'elfeed-tube-save
          :localleader
          :desc "Youtube fetch" "f" #'elfeed-tube-fetch)
    (use-package elfeed-tube-mpv
      :when (executable-find "mpv")
      :config
      (map! :map elfeed-show-mode-map
            "C-c C-f" #'elfeed-tube-mpv-follow-mode
            "C-c C-w" #'elfeed-tube-mpv-where
            :localleader
            :desc "MPV Follow mode" "f" #'elfeed-tube-mpv-follow-mode
            :desc "MPV Where" "w" #'elfeed-tube-mpv-where
            :desc "MPV Play" "p" #'elfeed-tube-mpv))))

