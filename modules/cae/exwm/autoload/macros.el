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

(defvar-local cae-exwm-mouse-frozen-position nil
  "Frozen cursor position when mouse is disabled. Format: (x . y)")

(defvar-local cae-exwm-mouse-freeze-timer nil
  "Timer for maintaining frozen cursor position.")

(defvar-local cae-exwm-space-spam-timer nil
  "Timer for spamming space key to EXWM window.")

(defvar-local cae-exwm-space-spam-buffer nil
  "Buffer associated with space spam timer.")

(defvar-local cae-exwm-space-spam-evil-state nil
  "Evil state when space spam was started.")
