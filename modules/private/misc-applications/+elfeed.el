;;; private/misc-applications/+elfeed.el -*- lexical-binding: t; -*-

(when (modulep! :app rss)
  (map! :leader :prefix +misc-applications-prefix
        "r" #'=rss)

  (after! elfeed
    (push elfeed-db-directory recentf-exclude))

  (use-package elfeed-tube
    :after elfeed
    :config
    (elfeed-tube-setup)
    :bind (:map elfeed-show-mode-map
           ("F" . elfeed-tube-fetch)
           ([remap save-buffer] . elfeed-tube-save)
           :map elfeed-search-mode-map
           ("F" . elfeed-tube-fetch)
           ([remap save-buffer] . elfeed-tube-save))))
