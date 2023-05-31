;;; private/misc-applications/+elfeed.el -*- lexical-binding: t; -*-

(when (modulep! :app rss)
  (map! :leader :prefix +misc-applications-prefix
        "r" #'=rss)

  ;; TODO Add starring

  (after! elfeed
    (push elfeed-db-directory recentf-exclude)
    (map! :map elfeed-show-mode-map
          "?" #'describe-mode
          :map elfeed-search-mode-map
          "?" #'describe-mode
          "q" #'+elfeed-quit)
    (defhydra cae-elfeed-hydra ()
      "filter"
      ("c" (elfeed-search-set-filter "@6-months-ago +cs") "cs")
      ("e" (elfeed-search-set-filter "@6-months-ago +emacs") "emacs")
      ("*" (elfeed-search-set-filter "@6-months-ago +star") "Starred")
      ;;("M" elfeed-toggle-star "Mark")
      ("A" (elfeed-search-set-filter "@6-months-ago") "All")
      ("T" (elfeed-search-set-filter "@1-day-ago") "Today")
      ("Q" +elfeed-quit "Quit Elfeed" :color blue)
      ("q" nil "quit" :color blue))
    (map! :map elfeed-search-mode-map
          "<f6>" #'cae-elfeed-hydra/body)

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
