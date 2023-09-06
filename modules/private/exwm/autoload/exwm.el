;;; private/exwm/autoload/evil.el -*- lexical-binding: t; -*-

(defvar +exwm-refocus-application--message nil)
(defvar +exwm-refocus-application--delay (+ exwm-input--update-focus-interval 0.011))
(defvar +exwm-refocus-application--timer nil)

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
      (add-hook! 'minibuffer-setup-hook
                 #'+exwm-refocus-application-minibuffer-quit-timer))
    (read-string "")))

(defun +exwm-refocus-application-minibuffer-quit-timer ()
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
                 (ignore-errors (throw 'exit #'ignore)))))
