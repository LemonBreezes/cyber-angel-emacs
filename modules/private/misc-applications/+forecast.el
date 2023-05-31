;;; private/misc-applications/forecast.el -*- lexical-binding: t; -*-

(use-package! forecast
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        "C-w" #'+forecast)
  :config
  (setq forecast-api-key "3952024acf85777d62f39869da12f853"
        forecast-units 'us
        forecast-language 'en)
  (map! :map forecast-mode-map
        "q" #'+forecast-quit
        "r" #'forecast-refresh))
