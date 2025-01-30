;;; cae/notifications/config.el -*- lexical-binding: t; -*-


(use-package! ednc
  :defer t :init
  (defun cae-ednc-load-h ()
    (and (require 'dbus nil t)
         (not (getenv "INSIDE_EXWM")) ; In EXWM I prefer using Dunst.
         (let ((path "/org/freedesktop/Notifications"))
           (dbus-ping (subst-char-in-string ?/ ?. (substring path 1)) path))
         (ednc-mode +1)))
  (run-with-idle-timer 1.5 nil #'cae-ednc-load-h)
  (add-hook 'ednc-notification-presentation-functions
            #'cae-ednc-show-notification-in-buffer)
  (defun cae-ednc-stack-notifications (&optional hide)
    (mapconcat (lambda (notification)
                 (let ((app-name (ednc-notification-app-name notification)))
                   (unless (member app-name hide)
                     (push app-name hide)
                     (ednc-format-notification notification))))
               (ednc-notifications) ""))
  (add-hook 'ednc-notification-presentation-functions
            (lambda (&rest _) (force-mode-line-update t)))
  ;; Otherwise `alert-send-notification' will block the UI.
  (advice-add #'alert-send-notification :around
              #'cae-ednc-wrap-async-call-process-a))
:config
(add-to-list 'global-mode-string
             '((:eval (cae-ednc-stack-notifications))))
(map! :map ednc-view-mode-map
      "n" #'next-line
      "p" #'previous-line)
