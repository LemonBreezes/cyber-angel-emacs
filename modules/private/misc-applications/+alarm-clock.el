;;; private/misc-applications/alarm-clock.el -*- lexical-binding: t; -*-

(use-package! alarm-clock
  :defer t
  :init
  (map! :leader
        :prefix +misc-applications-prefix
        (:prefix ("a" . "alarms")
         "a" #'alarm-clock-set
         "A" #'alarm-clock-list-view))
  :config
  (map! :leader
        :prefix +misc-applications-prefix
        (:prefix ("a" . "alarms")
         "k" #'alarm-clock-kill))
  (setq alarm-clock-cache-file
        (expand-file-name "alarm-clock.cache" doom-cache-dir))
  (alarm-clock--turn-autosave-on))
