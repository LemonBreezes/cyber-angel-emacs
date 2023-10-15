;;; lisp/cae-exwm.el -*- lexical-binding: t; -*-

(defvar cae-exwm-evil-initial-state-alist
  '(("kitty" . insert)))

(defun cae-exwm-exit-floating-mode-h ()
  (when (string= exwm-class-name "love")
    (exwm-floating--unset-floating (exwm--buffer->id (window-buffer)))))

(defun cae-exwm-evil-use-initial-state-h ()
  (when-let ((state (alist-get exwm-class-name cae-exwm-evil-initial-state-alist
                               nil nil #'string=)))
    (+log state)
    (pcase state
      ('normal (exwm-evil-normal-state))
      ('insert (exwm-evil-insert)))))

(add-hook 'exwm-manage-finish-hook #'cae-exwm-exit-floating-mode-h)
(add-hook 'exwm-manage-finish-hook #'cae-exwm-evil-use-initial-state-h)

(defmacro cae-exwm-app-runner (app-name app-title &optional state)
  `(lambda (arg)
     (interactive "P")
     (cae-exwm-start-app ,app-name ,app-title arg)))

(add-hook! 'exwm-init-hook
  (map! "s-v" (cae-exwm-app-runner browse-url-generic-program "Chrome")
        "s-f" (cmd! () (start-process "flameshot" nil "flameshot" "gui"))
        "s-t" (cae-exwm-app-runner "teams-for-linux" "Teams")
        "s-T" (cae-exwm-app-runner "tiled" "Tiled")
        "s-d" (cae-exwm-app-runner "discord" "Discord")
        "s-p" (cae-exwm-app-runner "pavucontrol" "Pavucontrol")
        "s-s" (cae-exwm-app-runner "signal-desktop" "Signal")
        "s-<return>" (cae-exwm-app-runner "kitty" "kitty")))
