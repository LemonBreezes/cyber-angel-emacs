;;; private/exwm/autoload/evil.el -*- lexical-binding: t; -*-

(defvar +exwm-refocus-application--message nil)
(defvar +exwm-refocus-application--delay (+ exwm-input--update-focus-interval 0.001))

;;;###autoload
(defun +exwm-refocus-application (&rest _)
  "Refocus input for the currently selected EXWM buffer, if any."
  (when (derived-mode-p 'exwm-mode)
    (run-at-time +exwm-refocus-application--delay nil #'+exwm-refocus-application--timer)))

(defun +exwm-refocus-application--timer ()
  (when (derived-mode-p 'exwm-mode)
    (setq +exwm-refocus-application--message (current-message))
    (advice-add #'+exwm-refocus-application :override #'ignore)
    (let ((state (bound-and-true-p evil-state)))
      (add-transient-hook! 'minibuffer-setup-hook
        (run-at-time +exwm-refocus-application--delay nil
                     (lambda ()
                       (run-at-time
                        0.0 nil
                        (lambda ()
                          (minibuffer-message +exwm-refocus-application--message)
                          (advice-remove #'+exwm-refocus-application #'ignore)
                          (pcase state
                            ('insert (exwm-evil-core-insert))
                            ('normal (exwm-evil-core-normal))
                            (_ nil))))
                       (ignore-errors (throw 'exit #'ignore))))))
    (read-string "")))

;;;###autoload
(defun +exwm-do-mouse-click (x y &optional button-num window-id)
  "Perform a mouse click at (window relative) position X and Y

By default BUTTON-NUM is ``1'' (i.e. main click) and the WINDOW-ID is the currently selected window."
  (let* ((button-index (intern (format "xcb:ButtonIndex:%d" (or button-num 1))))
         (button-mask (intern (format "xcb:ButtonMask:%d" (or button-num 1))))
         (window-id (or window-id (exwm--buffer->id
                                   (window-buffer (selected-window)))
                        (user-error "No window selected")))
         (button-actions `((xcb:ButtonPress . ,button-mask)
                           (xcb:ButtonRelease . 0))))
    (dolist (b-action button-actions)
      (xcb:+request exwm--connection
          (make-instance 'xcb:SendEvent
                         :propagate 0
                         :destination window-id
                         :event-mask xcb:EventMask:NoEvent
                         :event (xcb:marshal
                                 (make-instance (car b-action)
                                                :detail button-index
                                                :time xcb:Time:CurrentTime
                                                :root exwm--root
                                                :event window-id
                                                :child 0
                                                :root-x 0
                                                :root-y 0
                                                :event-x x
                                                :event-y y
                                                :state (cdr b-action)
                                                :same-screen 0)
                                 exwm--connection))))
    (xcb:flush exwm--connection)))

;;;###autoload
(defun +exwm-evil-do-left-click ()
  "Perform a left mouse click at the current cursor position."
  (interactive)
  (cl-destructuring-bind (mouse-x . mouse-y)
      (mouse-absolute-pixel-position)
    (if (provided-mode-derived-p
         (buffer-local-value 'major-mode
                             (window-buffer (window-at mouse-x mouse-y)))
         'exwm-mode)
        (progn
          (exwm-evil-insert)
          (+exwm-do-mouse-click mouse-x mouse-y))
      (call-interactively #'evil-mouse-drag-region))))
