;;; private/misc-applications/forecast.el -*- lexical-binding: t; -*-

(use-package! forecast
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        "w" #'+forecast)
  :config
  (setq forecast-api-key "3952024acf85777d62f39869da12f853"
        forecast-units 'us
        forecast-language 'en)
  (map! :map forecast-mode-map
        :n "q" #'+forecast-quit
        :n "r" #'forecast-refresh))
