;;; cae/notifications/config.el -*- lexical-binding: t; -*-

(defvar cae-dbus-notifications-supported-p nil
  "Whether the current system supports D-Bus notifications.")

(run-with-idle-timer
 1.5 nil
 (lambda ()
   (setq cae-dbus-notifications-supported-p
         (let ((path "/org/freedesktop/Notifications"))
           (dbus-ping (subst-char-in-string ?/ ?. (substring path 1)) path)))
   (if cae-dbus-notifications-supported-p
       (cae-ednc-load-h)
     ;; Use `alert' instead of `notifications-notify'.
     (advice-add 'notifications-notify :around #'cae-notifications-notify-advice))))

(use-package! ednc
  :defer t :init
  (add-hook 'ednc-notification-presentation-functions
            #'ednc-popup-presentation-function)
  ;;(add-hook 'ednc-notification-presentation-functions
  ;;          #'cae-ednc-show-notification-in-buffer)
  ;;(add-hook 'ednc-notification-presentation-functions
  ;;          (lambda (&rest _) (force-mode-line-update t)))
  :config
  (add-to-list 'global-mode-string
               '((:eval (cae-ednc-stack-notifications))))
  (setq alert-default-style 'libnotify)
  (map! :map ednc-view-mode-map
        "n" #'next-line
        "p" #'previous-line))

(use-package! alert
  :defer t :init
  (after! circe-notifications
    (setq circe-notifications-alert-style nil))
  ;; Some elisp packages use `notifications-notify' directly and provide no easy
  ;; way to use `alert'.
  (advice-add 'notifications-notify :around #'cae-notifications-notify-advice)
  :config
  ;; BUG Otherwise `alert-send-notification' will block the UI.
  (advice-add #'alert-send-notification :around
              #'cae-ednc-wrap-async-call-process-a)
  (setq alert-default-style
        (cond ((getenv "WSL_DISTRO_NAME")
               'toast)
              (t 'libnotify))))

(use-package! alert-toast
  :when (getenv "WSL_DISTRO_NAME")
  :after alert)
