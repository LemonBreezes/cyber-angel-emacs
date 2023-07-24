;;; private/misc-applications/+ednc.el -*- lexical-binding: t; -*-

(use-package! ednc
  :defer t
  :init
  (defun +ednc-load-h ()
    (and (require 'dbus nil t)
         (dbus-ping :system "org.freedesktop.DBus")
         (ednc-mode +1)))
  (run-with-idle-timer 1.5 nil #'+ednc-load-h)
  (map! :map +misc-applications-emacs-os-map
        "n" #'+ednc-show-notifications)
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-emacs-os-map
      "n" "Notifications"))
  :config
  (defun show-notification-in-buffer (old new)
    (let ((name (format "*Notification %d*" (ednc-notification-id (or old new)))))
      (with-current-buffer (get-buffer-create name)
        (if new (let ((inhibit-read-only t))
                  (if old (erase-buffer) (ednc-view-mode))
                  (set-buffer-multibyte nil)
                  (insert (concat (ednc-format-notification new)
                                  "\n"
                                  (ednc-format-notification new t)))
                  (delete-blank-lines)
                  (display-buffer (current-buffer)))
          (kill-buffer)))))
  (add-hook 'ednc-notification-presentation-functions #'show-notification-in-buffer)

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
  (map! :map ednc-view-mode-map
        "n" #'next-line
        "p" #'previous-line))
