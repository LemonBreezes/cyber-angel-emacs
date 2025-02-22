;;; cae/notifications/config.el -*- lexical-binding: t; -*-

(defvar cae-dbus-notifications-supported-p nil
  "Whether the current system supports D-Bus notifications.")

(defun cae-notifications-setup ()
  (setq cae-dbus-notifications-supported-p
        (let ((path "/org/freedesktop/Notifications"))
          (when (require 'dbus nil t)
            (dbus-ping (subst-char-in-string ?/ ?. (substring path 1)) path))))
  (if cae-dbus-notifications-supported-p
      (progn
        (advice-remove #'alert-send-notification :around
                       #'cae-notifications-wrap-async-call-process-a)
        (setq alert-default-style 'libnotify)
        (and (require 'dbus nil t)
             (not (getenv "INSIDE_EXWM")) ; In EXWM I prefer using Dunst.
             cae-dbus-notifications-supported-p
             (ednc-mode +1)))
    ;; Use `alert' instead of `notifications-notify'.
    (advice-add #'notifications-notify :around #'cae-notifications-notify-advice)))

(run-with-idle-timer 3 nil #'cae-notifications-setup)

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
  (setq alert-default-style
        (cond ((getenv "WSL_DISTRO_NAME")
               'toast)
              (t 'libnotify)))
  ;; BUG Otherwise `alert-send-notification' will block the UI.
  (advice-add #'alert-send-notification :around
              #'cae-notifications-wrap-async-call-process-a)

  (setq alert-default-style
        (cond ((getenv "WSL_DISTRO_NAME")
               (require ' alert-toast)
               'toast)
              ((or (display-graphic-p)
                   (featurep 'tty-child-frames))
               (alert-define-style 'child-frame
                                   :title "Display notification in a child frame popup"
                                   :notifier #'alert-child-frame-notify
                                   :remover #'alert-child-frame-remove)
               'child-frame)
              (t 'message))))
