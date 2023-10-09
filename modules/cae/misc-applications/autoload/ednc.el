;;; private/misc-applications/autoload/ednc.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ednc-show-notifications ()
  (interactive)
  (require 'ednc)
  (let ((buf (get-buffer-create ednc-log-name)))
    (pop-to-buffer buf)))

;;;###autoload
(defun +ednc-show-notification-in-buffer (old new)
  (let ((name (format "*Notification %d*" (ednc-notification-id (or old new)))))
    (with-current-buffer (get-buffer-create name)
      (if new (let ((inhibit-read-only t))
                (if old (erase-buffer) (ednc-view-mode))
                (set-buffer-multibyte nil)
                (insert (ednc-format-notification new t))
                (display-buffer (current-buffer)))
        (kill-buffer)))))

;;;###autoload
(defun +ednc-dismiss-all-notifications ()
  (interactive)
  (cl-mapcar (lambda (notification)
               (ednc-dismiss-notification notification))
             (ednc-notifications)))
