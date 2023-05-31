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
