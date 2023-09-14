;;; private/exwm/+notifications.el -*- lexical-binding: t; -*-

(use-package! ednc
  :defer t :init
  (defun +ednc-load-h ()
    (and (require 'dbus nil t)
         (ednc-mode +1)))
  (run-with-idle-timer 1.5 nil #'+ednc-load-h)
  ;; TODO Add keybindings.
  (add-hook 'ednc-notification-presentation-functions #'+ednc-show-notification-in-buffer)
  (defun +ednc-stack-notifications (&optional hide)
    (mapconcat (lambda (notification)
                 (let ((app-name (ednc-notification-app-name notification)))
                   (unless (member app-name hide)
                     (push app-name hide)
                     (ednc-format-notification notification))))
               (ednc-notifications) ""))
  (add-hook 'ednc-notification-presentation-functions
            (lambda (&rest _) (force-mode-line-update t)))
  :config
  (add-to-list 'global-mode-string
               '((:eval (+ednc-stack-notifications))))
  (map! :map ednc-view-mode-map
        "n" #'next-line
        "p" #'previous-line))
