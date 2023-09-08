;;; startup/dunst.el -*- lexical-binding: t; -*-

(when (executable-find "dunstctl")
  (after! alert
    (setq alert-default-style 'libnotify))
  (defvar startup/dunst-process nil)

  (defun startup/start-dunst ()
    (setq startup/dunst-process
          (start-process "dunst" " *startup/dunst*" "dunst" "-config"
                         (expand-file-name
                          (if (cae-dark-theme-p)
                              "dunstrc-dark"
                            "dunstrc-light")
                          +startup-config-dir))))

  (defun startup/kill-dunst ()
    (interactive)
    (when (process-live-p startup/dunst-process)
      (kill-process startup/dunst-process)))

  (defun startup/restart-dunst ()
    (interactive)
    (startup/kill-dunst)
    (startup/start-dunst))

  (add-hook 'circadian-after-load-theme-hook #'startup/restart-dunst)

  (map! :leader
        :prefix +startup-prefix
        (:prefix"n"
         :desc "Restart Dunst" "r" #'startup/restart-dunst
         :desc "Kill Dunst" "x" #'startup/kill-dunst
         :desc "Close notification" "c"
         (cmd! (let ((shell-file-name "/bin/sh"))
                 (quiet! (shell-command "dunstctl close" nil nil))))
         :desc "History pop" "h"
         (cmd! (let ((shell-file-name "/bin/sh"))
                 (quiet! (shell-command "dunstctl history-pop" nil nil))))
         :desc "Close all notifications" "C"
         (cmd! (let ((shell-file-name "/bin/sh"))
                 (quiet! (shell-command "dunstctl close-all" nil nil))))
         :desc "Show context menu" "."
         (cmd! (let ((shell-file-name "/bin/sh"))
                 (quiet! (shell-command "dunstctl context" nil nil))))))

  (if (process-live-p startup/dunst-process)
      (startup/restart-dunst)
    (startup/start-dunst)))
