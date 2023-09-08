;;; private/misc-applications/+ytel.el -*- lexical-binding: t; -*-

;; `empv' blows this package out of the water.

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
  (define-key ytel-mode-map (kbd "<return>") #'ytel-watch)
  (define-key ytel-mode-map (kbd "<f6>") #'+ytel-hydra/body))
