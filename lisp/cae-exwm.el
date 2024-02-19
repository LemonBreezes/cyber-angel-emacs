;;; lisp/cae-exwm.el -*- lexical-binding: t; -*-

(setq exwm-evil-initial-state-alist
      '(("kitty" . insert)))

(defun cae-exwm-exit-floating-mode-h ()
  (when (string= exwm-class-name "love")
    (exwm-floating--unset-floating (exwm--buffer->id (window-buffer)))))

(add-hook 'exwm-manage-finish-hook #'cae-exwm-exit-floating-mode-h)

(defmacro cae-exwm-app-runner (app-name app-title)
  `(lambda (arg)
     (interactive "P")
     (cae-exwm-start-app ,app-name ,app-title arg)))

(map! "s-v" (cae-exwm-app-runner browse-url-generic-program cae-generic-browser-name)
      "s-f" (cmd! () (start-process "flameshot" nil "flameshot" "gui"))
      "s-T" (cae-exwm-app-runner "teams-for-linux" "Teams")
      "s-t" (cae-exwm-app-runner "tiled" "Tiled")
      "s-d" (cae-exwm-app-runner "discord" "Discord")
      "s-p" (cae-exwm-app-runner "pavucontrol" "Pavucontrol")
      "s-s" (cae-exwm-app-runner "signal-desktop" "Signal")
      "s-e" #'+exwm-open-nested-emacs
      "s-E" #'+exwm-open-nested-vanilla-emacs
      "s-<return>" (cae-exwm-app-runner "kitty" "Kitty"))

(when (modulep! :ui hydra)
  (eval
   `(defhydra hydra-exwm-apps (:hint nil :color blue)
      "
EXWM Apps Launcher
_v_: [Browser]    _f_: Flameshot      _T_: Teams
_t_: Tiled        _d_: Discord        _p_: Pavucontrol
_s_: Signal       _e_: Emacs          _E_: Vanilla Emacs
_RET_: Kitty
"
      ("v" ,(cae-exwm-app-runner browse-url-generic-program cae-generic-browser-name))
      ("f" ,(cmd! () (start-process "flameshot" nil "flameshot" "gui")))
      ("T" ,(cae-exwm-app-runner "teams-for-linux" "Teams"))
      ("t" ,(cae-exwm-app-runner "tiled" "Tiled"))
      ("d" ,(cae-exwm-app-runner "discord" "Discord"))
      ("p" ,(cae-exwm-app-runner "pavucontrol" "Pavucontrol"))
      ("s" ,(cae-exwm-app-runner "signal-desktop" "Signal"))
      ("e" #'+exwm-open-nested-emacs)
      ("E" #'+exwm-open-nested-vanilla-emacs)
      ("RET" ,(cae-exwm-app-runner "kitty" "Kitty"))))

  ;; Replace the individual keybindings with a hydra
  (global-set-key (kbd "s-h") 'hydra-exwm-apps/body))
