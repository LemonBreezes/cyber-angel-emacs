;;; private/misc-applications/autoload/elfeed.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +elfeed-quit ()
  (interactive)
  (elfeed-db-save)
  (+workspace/delete +rss-workspace-name)
  (when (buffer-live-p elfeed-log-buffer-name)
    (kill-buffer elfeed-log-buffer-name)))

;;;###autoload
(defun +elfeed-toggle-log-buffer ()
  (interactive)
  (if (get-buffer-window elfeed-log-buffer-name)
      (delete-window (get-buffer-window elfeed-log-buffer-name))
    (pop-to-buffer elfeed-log-buffer-name)))

;;;###autoload (autoload 'cae-elfeed-hydra/body "private/misc-applications/autoload/elfeed" nil t)
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
