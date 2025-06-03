;;; cae/exwm/autoload/macros.el -*- lexical-binding: t; -*-

(defcustom cae-exwm-fake-type-delay 0.01
  "Delay in seconds between each character when fake typing."
  :type 'float
  :group 'exwm)

;;;###autoload
(defun cae-exwm-fake-type-clipboard-advanced ()
  "Fake type clipboard contents with proper handling of special characters."
  (interactive)
  (let ((clipboard-text (gui-get-selection 'CLIPBOARD 'STRING)))
    (if clipboard-text
        (if (derived-mode-p 'exwm-mode)
            (let ((total-chars (length clipboard-text))
                  (typed-chars 0))

              ;; Show progress
              (message "Typing %d characters..." total-chars)

              ;; Process each character
              (dolist (char (string-to-list clipboard-text))
                (pcase char
                  ;; Special characters
                  (?\n (exwm-input--fake-key 'return))
                  (?\t (exwm-input--fake-key 'tab))
                  (?\b (exwm-input--fake-key 'backspace))

                  ;; Characters requiring shift
                  ((guard (and (>= char ?A) (<= char ?Z)))
                   (exwm-input--fake-key char))

                  ;; Special symbols that might need shift
                  (?! (exwm-input--fake-key ?!))
                  (?@ (exwm-input--fake-key ?@))
                  (?# (exwm-input--fake-key ?#))
                  (?$ (exwm-input--fake-key ?$))
                  (?% (exwm-input--fake-key ?%))
                  (?^ (exwm-input--fake-key ?^))
                  (?& (exwm-input--fake-key ?&))
                  (?* (exwm-input--fake-key ?*))
                  (?\( (exwm-input--fake-key ?\())
                  (?\) (exwm-input--fake-key ?\)))

                  ;; Default: regular character
                  (_ (exwm-input--fake-key char)))

                ;; Update progress occasionally
                (setq typed-chars (1+ typed-chars))
                (when (zerop (% typed-chars 10))
                  (message "Typing... %d/%d" typed-chars total-chars))

                ;; Small delay
                (sit-for cae-exwm-fake-type-delay))

              (message "Done typing %d characters!" total-chars))
          (message "Not in an EXWM buffer!"))
      (message "Clipboard is empty!"))))

(defvar-local cae-exwm-mouse-disabled-p nil
  "Whether mouse input is disabled for the current EXWM window.")

;;;###autoload
(defun cae-exwm-toggle-mouse ()
  "Toggle mouse input for the current EXWM window using pointer grab."
  (interactive)
  (when (derived-mode-p 'exwm-mode)
    (if cae-exwm-mouse-disabled-p
        ;; Enable mouse - ungrab the pointer
        (progn
          (xcb:+request exwm--connection
              (make-instance 'xcb:UngrabPointer
                             :time xcb:Time:CurrentTime))
          (xcb:flush exwm--connection)
          (setq cae-exwm-mouse-disabled-p nil)
          (message "Mouse enabled for window: %s" exwm-title))
      ;; Disable mouse - grab the pointer
      (progn
        (xcb:+request exwm--connection
            (make-instance 'xcb:GrabPointer
                           :owner-events 0
                           :grab-window exwm--id
                           :event-mask 0
                           :pointer-mode xcb:GrabMode:Async
                           :keyboard-mode xcb:GrabMode:Async
                           :confine-to 0
                           :cursor 0
                           :time xcb:Time:CurrentTime))
        (xcb:flush exwm--connection)
        (setq cae-exwm-mouse-disabled-p t)
        (message "Mouse disabled for window: %s" exwm-title)))))
