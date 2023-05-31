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
  (define-key ytel-mode-map (kbd "RET") #'ytel-watch))

;; (define-key ytel-mode-map "y" #'ytel-watch)

;; youtube-comments

;; xFA25E/ytel-show
;; (use-package ytel-show
;; :after ytel
;; :bind (:map ytel-mode-map ("RET" . ytel-show))
;; )
