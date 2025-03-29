;;; cae/exwm/autoload/evil.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-exwm-load-special-bindings-h ()
  (cond ((and (stringp exwm-class-name)
              (string-match-p "discord" exwm-class-name))
         (cae-exwm-discord-mode +1))
        ((and (stringp exwm-class-name)
              (string-match-p "retroarch" exwm-class-name))
         (evil-emacs-state)
         (setq-local exwm-input-line-mode-passthrough nil
                     exwm-input-prefix-keys
                     (delq 'escape exwm-input-prefix-keys)))))
