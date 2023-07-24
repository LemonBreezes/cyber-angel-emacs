;;; private/misc-applications/alarm-clock.el -*- lexical-binding: t; -*-

(use-package! alarm-clock
  :defer t
  :init
  (map! :map +misc-applications-standalone-apps-map
        :prefix "a"
        "a" #'alarm-clock-set
        "A" #'alarm-clock-list-view)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-standalone-apps-map
      "a a" "set alarm"
      "a A" "list alarms"))
  :config
  (map! :leader
        :prefix +misc-applications-prefix
        (:prefix ("a" . "alarms")
         "k" #'alarm-clock-kill))
  (setq alarm-clock-cache-file
        (expand-file-name "alarm-clock.cache" doom-cache-dir))
  (alarm-clock--turn-autosave-on))
