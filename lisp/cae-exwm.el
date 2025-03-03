;;; lisp/cae-exwm.el -*- lexical-binding: t; -*-

(defvar cae-exwm--redshift-process nil
  "Process object for the running redshift instance.")

(defun cae-exwm-toggle-redshift ()
  "Toggle redshift on/off using location data from cae-location-data."
  (interactive)
  (if (and cae-exwm--redshift-process 
           (process-live-p cae-exwm--redshift-process))
      (progn
        ;; Kill the existing process
        (interrupt-process cae-exwm--redshift-process)
        (delete-process cae-exwm--redshift-process)
        (setq cae-exwm--redshift-process nil)
        ;; Reset screen temperature
        (start-process "redshift-reset" nil "redshift" "-x")
        (message "Redshift turned off"))
    (let* ((lat (cdr (assq 'latitude cae-location-data)))
           (lon (cdr (assq 'longitude cae-location-data)))
           (lat-str (number-to-string lat))
           (lon-str (number-to-string lon)))
      ;; Start new redshift process and store the process object
      (setq cae-exwm--redshift-process 
            (start-process "redshift" nil "redshift" "-l" 
                           (concat lat-str ":" lon-str)))
      (message "Redshift turned on for %s" (cdr (assq 'name cae-location-data))))))

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
      "s-e" #'cae-exwm-open-nested-emacs
      "s-E" #'cae-exwm-open-nested-vanilla-emacs
      "s-D" #'cae-exwm-open-nested-vanilla-doom-emacs
      "s-<return>" (cae-exwm-app-runner "kitty" "Kitty")
      "s-S-<return>" #'cae-open-eshell-in-new-workspace)

(when (modulep! :ui hydra)
  (eval
   `(defhydra hydra-exwm-apps (:hint nil :color blue)
      "
EXWM Apps Launcher
_v_: [Browser]    _f_: Flameshot      _T_: Teams
_t_: Tiled        _d_: Discord        _p_: Pavucontrol
_s_: Signal       _e_: Emacs          _E_: Vanilla Emacs  _D_: Vanilla Doom Emacs
_RET_: Kitty      _S-RET_: Eshell     _r_: Toggle Redshift
"
      ("v" ,(cae-exwm-app-runner browse-url-generic-program cae-generic-browser-name))
      ("f" ,(cmd! () (start-process "flameshot" nil "flameshot" "gui")))
      ("T" ,(cae-exwm-app-runner "teams-for-linux" "Teams"))
      ("t" ,(cae-exwm-app-runner "tiled" "Tiled"))
      ("d" ,(cae-exwm-app-runner "discord" "Discord"))
      ("p" ,(cae-exwm-app-runner "pavucontrol" "Pavucontrol"))
      ("s" ,(cae-exwm-app-runner "signal-desktop" "Signal"))
      ("e" #'cae-exwm-open-nested-emacs)
      ("E" #'cae-exwm-open-nested-vanilla-emacs)
      ("D" #'cae-exwm-open-nested-vanilla-doom-emacs)
      ("RET" ,(cae-exwm-app-runner "kitty" "Kitty"))
      ("S-RET" #'cae-open-eshell-in-new-workspace)
      ("r" #'cae-exwm-toggle-redshift)))

  ;; Replace the individual keybindings with a hydra
  (global-set-key (kbd "s-h") 'hydra-exwm-apps/body))
