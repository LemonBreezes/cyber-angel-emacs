;;; private/exwm/autoload/evil.el -*- lexical-binding: t; -*-

(defvar +exwm-refocus-application--message "")
(defvar +exwm-refocus-application--delay (+ exwm-input--update-focus-interval 0.0011))
(defvar +exwm-refocus-application--timer nil)
(defvar +exwm-refocus-application--last-time 0)
(defvar +exwm-refocus-application--last-state nil)

;;;###autoload
(defun +exwm-refocus-application (&rest _)
  "Refocus input for the currently selected EXWM buffer, if any."
  (when (and (derived-mode-p 'exwm-mode)
             (not (memq +exwm-refocus-application--timer
                        timer-list))
             (> (float-time) (+ +exwm-refocus-application--last-time +exwm-refocus-application--delay)))
    (run-at-time +exwm-refocus-application--delay nil #'+exwm-refocus-application--timer)))

(defun +exwm-refocus-application--timer ()
  (when (derived-mode-p 'exwm-mode)
    (setq +exwm-refocus-application--message (current-message))
    (let ((+exwm-refocus-application--last-state (bound-and-true-p evil-state)))
      (minibuffer-with-setup-hook
          #'+exwm-refocus-application-minibuffer-quit-timer
        (read-string "")))))

(defun +exwm-refocus-application-minibuffer-quit-timer ()
  (setq +exwm-refocus-application--timer
        (run-at-time +exwm-refocus-application--delay nil
                     (lambda ()
                       (run-at-time
                        0.0 nil
                        (lambda ()
                          (when +exwm-refocus-application--message
                            (minibuffer-message +exwm-refocus-application--message))
                          (pcase +exwm-refocus-application--last-state
                            ('insert (exwm-evil-core-insert))
                            ('normal (exwm-evil-core-normal))
                            (_ nil))))
                       (when (minibufferp)
                         (setq +exwm-refocus-application--last-time (float-time))
                         (throw 'exit nil))))))
