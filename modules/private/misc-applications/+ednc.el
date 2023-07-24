;;; private/misc-applications/+ednc.el -*- lexical-binding: t; -*-

(use-package! ednc
  :defer t
  :init
  (defun +ednc-load-h ()
    (and (require 'dbus nil t)
         (ednc-mode +1)))
  (run-with-idle-timer 1.5 nil #'+ednc-load-h)
  (map! :map +misc-applications-emacs-os-map
        "ns" #'+ednc-show-notifications
        "nd" #'+ednc-dismiss-all-notifications)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-emacs-os-map
      "n" "notifications"
      "ns" "show notifications"
      "nd" "dismiss all notifications"))
  :config
  (defun +ednc-show-notification-in-buffer (old new)
    (let ((name (format "*Notification %d*" (ednc-notification-id (or old new)))))
      (with-current-buffer (get-buffer-create name)
        (if new (let ((inhibit-read-only t))
                  (if old (erase-buffer) (ednc-view-mode))
                  (set-buffer-multibyte nil)
                  (insert (ednc-format-notification new t))
                  (delete-blank-lines)
                  (display-buffer (current-buffer)))
          (kill-buffer)))))
  (defun +ednc-dismiss-all-notifications ()
    (interactive)
    (cl-mapcar (lambda (notification)
                 (ednc-dismiss-notification notification))
               (ednc-notifications)))
  (add-hook 'ednc-notification-presentation-functions #'+ednc-show-notification-in-buffer)

  (defun +ednc-stack-notifications (&optional hide)
    (mapconcat (lambda (notification)
                 (let ((app-name (ednc-notification-app-name notification)))
                   (unless (member app-name hide)
                     (push app-name hide)
                     (ednc-format-notification notification))))
               (ednc-notifications) ""))
  (add-to-list 'global-mode-string
               '((:eval (+ednc-stack-notifications))))
  (add-hook 'ednc-notification-presentation-functions
            (lambda (&rest _) (force-mode-line-update t)))
  (map! :map ednc-view-mode-map
        "n" #'next-line
        "p" #'previous-line))
