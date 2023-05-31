;;; private/misc-applications/+ednc.el -*- lexical-binding: t; -*-

(use-package! ednc
  :when (not (and (string-suffix-p "-android" system-configuration)
                  (executable-find "termux-clipboard-get")))
  :config
  (ednc-mode +1)
  (defun stack-notifications (&optional hide)
    (mapconcat (lambda (notification)
                 (let ((app-name (ednc-notification-app-name notification)))
                   (unless (member app-name hide)
                     (push app-name hide)
                     (ednc-format-notification notification))))
               (ednc-notifications) ""))
  (add-to-list 'global-mode-string
               '((:eval (stack-notifications))))
  (add-hook 'ednc-notification-presentation-functions
            (lambda (&rest _) (force-mode-line-update t)))
  (map! :leader
        :prefix "o"
        :desc "Notifications" "n" #'+ednc-show-notifications)
  (map! :map ednc-view-mode-map
        "n" #'next-line
        "p" #'previous-line))
