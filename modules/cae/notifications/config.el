;;; cae/notifications/config.el -*- lexical-binding: t; -*-

;; BUG Otherwise `alert-send-notification' will block the UI.
(advice-add #'alert-send-notification :around
            #'cae-notifications-wrap-async-call-process-a)

;;(cae-check-dbus-and-notifications-async
;; (lambda (dbus-enabled notifications-daemon-present)
;;   (if dbus-enabled
;;       (if notifications-daemon-present
;;           (ignore)
;;         (ednc-mode +1))
;;     (setq alert-default-style 'libnotify)
;;     ;; Use `alert' instead of `notifications-notify'.
;;     (advice-add #'notifications-notify :around #'cae-notifications-notify-advice))))

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
