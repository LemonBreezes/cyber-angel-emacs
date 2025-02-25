;;; cae/misc-applications/autoload/ednc.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-ednc-show-notifications ()
  (interactive)
  (require 'ednc)
  (let ((buf (get-buffer-create ednc-log-name)))
    (pop-to-buffer buf)))

;;;###autoload
(defun cae-ednc-show-notification-in-buffer (old new)
  (let ((name (format "*Notification %d*" (ednc-notification-id (or old new)))))
    (with-current-buffer (get-buffer-create name)
      (if new (let ((inhibit-read-only t))
                (if old (erase-buffer) (ednc-view-mode))
                (set-buffer-multibyte nil)
                (insert (ednc-format-notification new t))
                (display-buffer (current-buffer)))
        (kill-buffer)))))

;;;###autoload
(defun cae-ednc-dismiss-all-notifications ()
  (interactive)
  (cl-mapcar (lambda (notification)
               (ednc-dismiss-notification notification))
             (ednc-notifications)))

;;;###autoload
(defun cae-ednc-toggle-notifications ()
  (interactive)
  (require 'ednc)
  (cond ((ednc-notifications)
         (cae-ednc-dismiss-all-notifications))
        ((get-buffer-window ednc-log-name)
         (delete-window (get-buffer-window ednc-log-name)))
        (t (cae-ednc-show-notifications))))

;;;###autoload
(defun cae-ednc-stack-notifications (&optional hide)
  (mapconcat (lambda (notification)
               (let ((app-name (ednc-notification-app-name notification)))
                 (unless (member app-name hide)
                   (push app-name hide)
                   (ednc-format-notification notification))))
             (ednc-notifications) ""))

;;;###autoload
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

;;(cae-check-dbus-and-notifications-async
;; (lambda (dbus-enabled notifications-present)
;;   (if dbus-enabled
;;       (message "DBus is enabled. Notifications daemon present? %s" notifications-present)
;;     (message "DBus does not appear to be enabled."))))
