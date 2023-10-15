;;; lisp/cae-exwm.el -*- lexical-binding: t; -*-

(defun +exwm-exit-floating-mode-h ()
  (when (string= exwm-class-name "love")
    (exwm-floating--unset-floating (exwm--buffer->id (window-buffer)))))

(add-hook 'exwm-manage-finish-hook #'+exwm-exit-floating-mode-h)

(add-hook! 'exwm-init-hook
  (map! "s-v" (cae-exwm-app-runner browse-url-generic-program "Chrome")
        "s-f" (cmd! () (start-process "flameshot" nil "flameshot" "gui"))
        "s-t" (cae-exwm-app-runner "teams-for-linux" "Teams")
        "s-T" (cae-exwm-app-runner "tiled" "Tiled")
        "s-d" (cae-exwm-app-runner "discord" "Discord")
        "s-p" (cae-exwm-app-runner "pavucontrol" "Pavucontrol")
        "s-s" (cae-exwm-app-runner "signal-desktop" "Signal")
        "s-<return>" (cae-exwm-app-runner "kitty" "kitty")))
