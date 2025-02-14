;;; cae/notifications/config.el -*- lexical-binding: t; -*-

(use-package! ednc
  :defer t :init
  (defun cae-ednc-load-h ()
    (and (require 'dbus nil t)
         (not (getenv "INSIDE_EXWM"))   ; In EXWM I prefer using Dunst.
         (let ((path "/org/freedesktop/Notifications"))
           (dbus-ping (subst-char-in-string ?/ ?. (substring path 1)) path))
         (ednc-mode +1)))
  (run-with-idle-timer 1.5 nil #'cae-ednc-load-h)
  (add-hook 'ednc-notification-presentation-functions
            #'cae-ednc-show-notification-in-buffer)
  (add-hook 'ednc-notification-presentation-functions
            (lambda (&rest _) (force-mode-line-update t)))
  :config
  (add-to-list 'global-mode-string
               '((:eval (cae-ednc-stack-notifications))))
  (map! :map ednc-view-mode-map
        "n" #'next-line
        "p" #'previous-line))

(use-package! alert
  :defer t :config
  ;; BUG Otherwise `alert-send-notification' will block the UI.
  (advice-add #'alert-send-notification :around
              #'cae-ednc-wrap-async-call-process-a)
  (after! alert
    (setq alert-default-style 'libnotify))
  )
