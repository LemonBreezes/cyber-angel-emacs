;;; cae/notifications/config.el -*- lexical-binding: t; -*-

;; BUG Otherwise `alert-send-notification' will block the UI.
(advice-add #'alert-send-notification :around
            #'cae-notifications-wrap-async-call-process-a)

(defun cae-check-dbus-and-notifications-async (callback)
  "Asynchronously check if DBus is enabled and if a notifications daemon is running.
CALLBACK is a function that takes two arguments:
  (DBUS-ENABLED NOTIFICATIONS-DAEMON-PRESENT).

If DBus isn’t enabled, the callback is invoked with nil for both arguments.
If DBus is enabled, then we issue an asynchronous call to ListNames on DBus
and check if \"org.freedesktop.Notifications\" is among the registered names."
  ;; Use a timer to schedule this check asynchronously.
  (let ((dbus-ok nil))
    ;; First, try to ping DBus. dbus-ping is synchronous,
    ;; so we call it inside our asynchronous lambda.
    (condition-case err
        (progn
          (dbus-ping :session)
          (setq dbus-ok t))
      (error
       (setq dbus-ok nil)))
    (if (not dbus-ok)
        ;; DBus is not enabled; immediately invoke the callback.
        (funcall callback nil nil)
      ;; DBus is enabled; now check for a notifications daemon.
      (dbus-call-method
       :session
       "org.freedesktop.DBus"
       "/org/freedesktop/DBus"
       "org.freedesktop.DBus"
       "ListNames"
       ;; :reply-function is called with the list of names.
       :reply-function (lambda (names)
                         (let ((notif-found (cl-some (lambda (name)
                                                       (string= name "org.freedesktop.Notifications"))
                                                     names)))
                           (funcall callback t (if notif-found t nil))))
       ;; In case of error in the asynchronous call,
       ;; assume that the notifications daemon isn’t present.
       :error-function (lambda (error)
                         (funcall callback t nil))))))

(cae-check-dbus-and-notifications-async
 (lambda (dbus-enabled notifications-daemon-present)
   (if dbus-enabled
       (progn
         (setq alert-default-style 'libnotify)
         (unless notifications-daemon-present
           (ednc-mode +1)))
     ;; Use `alert' instead of `notifications-notify'.
     (advice-add #'notifications-notify :around #'cae-notifications-notify-advice))))

(use-package! ednc
  :when (eq system-type 'gnu/linux)
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
  :config
  (setq alert-default-style
        (cond ((getenv "WSL_DISTRO_NAME")
               'toast)
              (t 'libnotify)))

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
