;;; private/exwm/autoload/evil.el -*- lexical-binding: t; -*-

(defvar +exwm-refocus-application--message "")
(defvar +exwm-refocus-application--delay exwm-input--update-focus-interval)
(defvar +exwm-refocus-application--timer nil)
(defvar +exwm-refocus-application--last-time 0)
;;;###autoload
(defun +exwm-refocus-application (&rest _)
  "Refocus input for the currently selected EXWM buffer, if any."
  (when (and (derived-mode-p 'exwm-mode)
             (> (float-time) (+ +exwm-refocus-application--last-time
                                (* 5 +exwm-refocus-application--delay))))
    (run-at-time +exwm-refocus-application--delay nil #'+exwm-refocus-application--timer)
    (setq +exwm-refocus-application--last-time (float-time))))

(defun +exwm-refocus-application--timer ()
  (when (derived-mode-p 'exwm-mode)
    (setq +exwm-refocus-application--message (current-message))
    (let ((state (bound-and-true-p evil-state)))
      (minibuffer-with-setup-hook
          (lambda ()
            (run-at-time +exwm-refocus-application--delay nil
             (lambda ()
               (run-at-time
                0.0 nil
                (lambda ()
                  (minibuffer-message "")
                  (pcase state
                    ('insert (exwm-evil-core-insert))
                    ('normal (exwm-evil-core-normal))
                    (_ nil))))
               (when (minibufferp)
                 (throw 'exit nil)))))
        (read-string "")))))
