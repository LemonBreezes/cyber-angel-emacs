;;; cae/notifications/autoload/alert-child-frame.el -*- lexical-binding: t; -*-

(require 'alert)
(require 'posframe)

;;;###autoload
(defun alert-child-frame-notify (info)
  "Render alert notification INFO as a child-frame popup using posframe.
INFO is a plist containing at least the keys :message and optionally :title.
If the alert is marked persistent in INFO then no auto-timeout is provided;
otherwise the frame disappears after `alert-fade-time' seconds."
  (let* ((buf (generate-new-buffer (format "*alert-child-frame: %s*"
                                           (or (plist-get info :title) "alert"))))
         (title   (plist-get info :title))
         (message (plist-get info :message))
         (content (if title (format "%s\n%s" title message) message))
         ;; use popup width and height if defined; otherwise defaults:
         (width  (if (and (boundp 'ednc-popup-width) ednc-popup-width)
                     ednc-popup-width
                   40))
         (height (if (and (boundp 'ednc-popup-max-height) ednc-popup-max-height)
                     ednc-popup-max-height
                   5))
         (timeout (if (plist-get info :persistent)
                      nil ;; no timeout if persistent
                    alert-fade-time)))
    (with-current-buffer buf
      (erase-buffer)
      (insert content)
      (setq-local cursor-type nil))
    (posframe-show buf
                   :timeout timeout
                   :width width
                   :height height
                   :border-width (if (and (boundp 'ednc-popup-border-width)
                                          ednc-popup-border-width)
                                     ednc-popup-border-width
                                   1)
                   :poshandler 'posframe-poshandler-frame-bottom-center)
    ;; Save a reference (in INFO) so that the removal function can hide it:
    (plist-put info :child-frame-buffer buf)
    buf))

;;;###autoload
(defun alert-child-frame-remove (info)
  "Remove the child-frame popup for an alert described by INFO.
This function hides the posframe that was created by `alert-child-frame-notify'
and then, after a short delay, kills its buffer so that the child frame remains
visible (in its hidden state) for a moment instead of being deleted immediately."
  (let ((buf (plist-get info :child-frame-buffer)))
    ;; Debugging it not working:
    (message "HELLO %s" buf)
    (when (and buf (buffer-live-p buf))
      (posframe-hide buf))))

;; Usage example:
;; (alert "This is a test notification" :title "Test" :style 'child-frame)
