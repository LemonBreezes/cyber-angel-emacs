;;; private/misc-applications/+ytel.el -*- lexical-binding: t; -*-

(use-package! ytel
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        "y" #'ytel)
  :config
  (setq ytel-invidious-api-url (or (doom-store-get 'ytel-invidious-api-url)
                                   "https://invidious.snopyta.org/"))
  (define-key ytel-mode-map (kbd "RET") #'ytel-watch)
  (define-key ytel-mode-map (kbd "<return>") #'ytel-watch))
