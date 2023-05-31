;;; private/misc-applications/+elfeed.el -*- lexical-binding: t; -*-

(when (modulep! :app rss)
  (map! :leader :prefix +misc-applications-prefix
        "r" #'=rss)

  (after! elfeed
    (defalias 'elfeed-toggle-star
      (elfeed-expose #'elfeed-search-toggle-all 'star))
    (push elfeed-db-directory recentf-exclude)
    (map! :map elfeed-show-mode-map
          "?" #'describe-mode
          :map elfeed-search-mode-map
          "?" #'describe-mode
          "q" #'+elfeed-quit)
    (set-popup-rule! (format "^%s$" (regexp-quote elfeed-log-buffer-name))
      :size 0.3 :side 'right :select nil :quit t :ttl nil)
    (when (modulep! :ui hydra)
      (defhydra cae-elfeed-hydra (:color pink :foreign-keys run)
        ("<f6>" nil "Exit" :exit t)
        ("E" (elfeed-search-set-filter "@6-months-ago +emacs") "emacs" :column "Custom filters")
        ("Y" (elfeed-search-set-filter "@6-months-ago +tube") "youtube" :column "Custom filters")
        ("*" (elfeed-search-set-filter "@6-months-ago +star") "Starred" :column "Custom filters")
        ("a" (elfeed-search-set-filter "@6-months-ago") "All" :column "Custom filters")
        ("t" (elfeed-search-set-filter "@1-day-ago") "Today" :column "Custom filters")
        ("s" elfeed-search-live-filter "Live filter" :column "Filter")
        ("S" elfeed-search-set-filter "Set filter" :column "Filter")
        ("c" elfeed-search-clear-filter "Clear filter" :column "Filter")
        ("RET" elfeed-search-show-entry "Show entry" :column "Navigation")
        ("b" elfeed-search-browse-url "Open entry" :column "Navigation")
        ("n" next-line "Next line" :column "Navigation")
        ("p" previous-line "Previous line" :column "Navigation")
        ("<" elfeed-search-first-entry "First entry" :column "Navigation")
        (">" elfeed-search-last-entry "Last entry" :column "Navigation")
        ("y" elfeed-search-yank "Yank" :column "Navigation")
        ("+" elfeed-search-tag-all "Tag all" :column "Tag")
        ("-" elfeed-search-untag-all "Untag all" :column "Tag")
        ("m" elfeed-toggle-star "Star" :column "Tag")
        ("r" elfeed-search-untag-all-unread "Untag all unread" :column "Tag")
        ("u" elfeed-search-tag-all-unread "Tag all unread" :column "Tag")
        ("F" elfeed-tube-fetch "Fetch" :column "Tube")
        ("C-x C-s" elfeed-tube-save "Save" :column "Tube")
        ("g" elfeed-search-update--force "Update" :column "Misc")
        ("l" +elfeed-toggle-log-buffer "Toggle log buffer" :column "Misc")
        ("q" +elfeed-quit "Quit Elfeed" :color blue))
      (map! :map elfeed-search-mode-map
            "<f6>" #'cae-elfeed-hydra/body
            ;; Elfeed maps `h' to `describe-mode', which is not as good.
            "h" #'cae-elfeed-hydra/body))

    (use-package elfeed-tube
      :after elfeed
      :config
      (elfeed-tube-setup)
      (map! :map elfeed-show-mode-map
            "F" #'elfeed-tube-fetch
            [remap save-buffer] #'elfeed-tube-save
            :map elfeed-search-mode-map
            "F" #'elfeed-tube-fetch
            [remap save-buffer] #'elfeed-tube-save))))
