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
      (pretty-hydra-define cae-elfeed-hydra (:color pink :foreign-keys run)
        ("Custom filters"
         (("E" (elfeed-search-set-filter "@6-months-ago +emacs") "emacs")
          ("Y" (elfeed-search-set-filter "@6-months-ago +tube") "youtube")
          ("*" (elfeed-search-set-filter "@6-months-ago +star") "Starred")
          ("a" (elfeed-search-set-filter "@6-months-ago") "All")
          ("t" (elfeed-search-set-filter "@1-day-ago") "Today"))
         "Filter"
         (("s" elfeed-search-live-filter "Live filter")
          ("S" elfeed-search-set-filter "Set filter")
          ("c" elfeed-search-clear-filter "Clear filter"))
         "Navigation"
         (("RET" elfeed-search-show-entry "Show entry")
          ("b" elfeed-search-browse-url "Open entry")
          ("n" next-line "Next line")
          ("p" previous-line "Previous line")
          ("<" elfeed-search-first-entry "First entry")
          (">" elfeed-search-last-entry "Last entry")
          ("y" elfeed-search-yank "Yank"))
         "Tag"
         (("+" elfeed-search-tag-all "Tag all")
          ("-" elfeed-search-untag-all "Untag all")
          ("m" elfeed-toggle-star "Star")
          ("r" elfeed-search-untag-all-unread "Untag all unread")
          ("u" elfeed-search-tag-all-unread "Tag all unread"))
         "Tube"
         (("F" elfeed-tube-fetch "Fetch")
          ("C-x C-s" elfeed-tube-save "Save"))
         "Misc"
         (("g" elfeed-search-update--force "Update")
          ("l" +elfeed-toggle-log-buffer "Toggle log buffer")
          ("Q" +elfeed-quit "Quit Elfeed" :color blue)
          ("q" nil "quit" :color blue))))
      (map! :map elfeed-search-mode-map
            "<f6>" #'cae-elfeed-hydra/body
            "h" #'cae-elfeed-hydra))

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
