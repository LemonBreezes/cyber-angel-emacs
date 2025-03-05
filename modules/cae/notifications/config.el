;;; cae/notifications/config.el -*- lexical-binding: t; -*-

;; BUG Otherwise `alert-send-notification' will block the UI.
(cae-advice-add #'alert-send-notification :around
            #'cae-notifications-wrap-async-call-process-a)

(cae-advice-add #'notifications-notify :around #'cae-notifications-notify-advice)
(when (require 'dbus nil t)
  (dbus-call-method-asynchronously
   :session
   "org.freedesktop.DBus"
   "/org/freedesktop/DBus"
   "org.freedesktop.DBus"
   "ListNames"
   (lambda (names)
     (let ((notif-found (cl-some (lambda (name)
                                   (string= name "org.freedesktop.Notifications"))
                                 names)))
       (advice-remove #'notifications-notify #'cae-notifications-notify-advice)
       (setq alert-default-style 'notifications)
       (unless notif-found
         (ednc-mode +1))))))

(use-package! ednc
  :when (eq system-type 'gnu/linux)
  :defer t :config
  (setq alert-default-style 'libnotify)
  (map! :map ednc-view-mode-map
        "n" #'next-line
        "p" #'previous-line))

(use-package! alert
  :defer t :config
  (setq alert-default-style
        (cond ((eq alert-default-style 'notifications)
               'notifications)
              ((getenv "WSL_DISTRO_NAME")
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
