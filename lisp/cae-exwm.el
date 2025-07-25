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
      "s-V" #'cae-exwm-run-vm
      "s-f" (cmd! () (start-process "flameshot" nil "flameshot" "gui"))
      ;;"s-T" (cae-exwm-app-runner "teams-for-linux" "Teams")
      "s-t" (cae-exwm-app-runner "tiled" "Tiled")
      "s-d" (cae-exwm-app-runner "discord" "Discord")
      "s-p" (cae-exwm-app-runner "pavucontrol" "Pavucontrol")
      "s-s" (cae-exwm-app-runner "signal-desktop" "Signal")
      "s-e" #'cae-exwm-open-nested-emacs
      "s-E" #'cae-exwm-open-nested-vanilla-emacs
      "s-D" #'cae-exwm-open-nested-vanilla-doom-emacs
      "s-r" #'cae-exwm-toggle-redshift
      "s-X" #'cae-exwm-lock-screen
      "s-x" #'cae-exwm-create-black-screen
      "s-<return>" (cae-exwm-app-runner "kitty" "Kitty")
      "s-S-<return>" #'cae-open-eshell-in-new-workspace)

;; Brightness control keys.
(global-set-key (kbd "<XF86MonBrightnessDown>") #'cae-exwm-decrease-brightness)
(global-set-key (kbd "<XF86MonBrightnessUp>") #'cae-exwm-increase-brightness)

;; Keyboard brightness control keys.
(global-set-key (kbd "<XF86KbdBrightnessDown>") #'cae-exwm-decrease-keyboard-brightness)
(global-set-key (kbd "<XF86KbdBrightnessUp>") #'cae-exwm-increase-keyboard-brightness)

;; Audio control keys.
(global-set-key (kbd "<XF86AudioRaiseVolume>") #'cae-exwm-audio-raise-volume)
(global-set-key (kbd "<XF86AudioLowerVolume>") #'cae-exwm-audio-lower-volume)
(global-set-key (kbd "<XF86AudioMicMute>") #'cae-exwm-audio-toggle-mic-mute)

(when (modulep! :ui hydra)
  (eval
   `(defhydra hydra-exwm-apps (:hint nil :color blue)
      "
EXWM Apps Launcher
_v_: [Browser]    _f_: Flameshot      _V_: VM (VMware/VirtualBox)
_t_: Tiled        _d_: Discord        _p_: Pavucontrol
_s_: Signal       _e_: Emacs          _E_: Vanilla Emacs    _D_: Vanilla Doom Emacs
_RET_: Kitty      _S-RET_: Eshell     _r_: Toggle Redshift  _l_: Lock Screen    _x_: Black Screen
"
      ("v" ,(cae-exwm-app-runner browse-url-generic-program cae-generic-browser-name))
      ("f" ,(cmd! () (start-process "flameshot" nil "flameshot" "gui")))
      ;;("T" ,(cae-exwm-app-runner "teams-for-linux" "Teams"))
      ("t" ,(cae-exwm-app-runner "tiled" "Tiled"))
      ("d" ,(cae-exwm-app-runner "discord" "Discord"))
      ("p" ,(cae-exwm-app-runner "pavucontrol" "Pavucontrol"))
      ("s" ,(cae-exwm-app-runner "signal-desktop" "Signal"))
      ("e" #'cae-exwm-open-nested-emacs)
      ("E" #'cae-exwm-open-nested-vanilla-emacs)
      ("D" #'cae-exwm-open-nested-vanilla-doom-emacs)
      ("RET" ,(cae-exwm-app-runner "kitty" "Kitty"))
      ("S-RET" #'cae-open-eshell-in-new-workspace)
      ("r" #'cae-exwm-toggle-redshift)
      ("l" #'cae-exwm-lock-screen)
      ("x" #'cae-exwm-create-black-screen)
      ("V" #'cae-exwm-run-vm)))

  ;; Replace the individual keybindings with a hydra
  (global-set-key (kbd "s-h") 'hydra-exwm-apps/body))

(add-hook! 'exwm-manage-finish-hook :depth 1
  (defun cae-exwm-load-special-bindings-h ()
    (cond ((and (stringp exwm-class-name)
                (string-match-p "discord" exwm-class-name))
           (evil-local-set-key 'normal (kbd "J")
                               (cmd! ()
                                     (exwm-input--fake-key ?\C-k)
                                     (exwm-evil-insert)))
           (evil-local-set-key 'normal (kbd "C-k")
                               (cmd! ()
                                     (exwm-input--fake-key ?\C-k)
                                     (exwm-evil-insert)))
           (evil-local-set-key 'normal (kbd "<return>")
                               (cmd! ()
                                     (exwm-input--fake-key 'return))))
          ((and (stringp exwm-class-name)
                (string-match-p "retroarch" exwm-class-name))
           (evil-emacs-state)
           (setq-local exwm-input-line-mode-passthrough nil
                       exwm-input-prefix-keys
                       (delq 'escape exwm-input-prefix-keys))))))

(when (and cae-config-finished-loading exwm--connection)
  (dolist (buf (buffer-list))
    (when
        (eq (buffer-local-value 'major-mode buf) 'exwm-mode)
      (with-current-buffer buf
        (cae-exwm-load-special-bindings-h)))))

(when (string= (system-name) "s")
  (setq exwm-randr-workspace-monitor-plist
        '(0 "HDMI-1"
          1 "eDP-1"))
  (setq exwm-workspace-number 2))

(defadvice! cae-exwm-disable-key-chord-in-exwm (first-char)
  :before-until #'key-chord-input-method
  (when (derived-mode-p 'exwm-mode)
    (list first-char)))
