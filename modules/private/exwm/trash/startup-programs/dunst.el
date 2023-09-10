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

  (map! :map +startup-applications-map
        (:prefix "n"
         "r" #'startup/restart-dunst
         "x" #'startup/kill-dunst
         "c" (cmd! (let ((shell-file-name "/bin/sh"))
                     (quiet! (shell-command "dunstctl close" nil nil))))
         "h" (cmd! (let ((shell-file-name "/bin/sh"))
                     (quiet! (shell-command "dunstctl history-pop" nil nil))))
         "C" (cmd! (let ((shell-file-name "/bin/sh"))
                     (quiet! (shell-command "dunstctl close-all" nil nil))))
         "." (cmd! (let ((shell-file-name "/bin/sh"))
                     (quiet! (shell-command "dunstctl context" nil nil))))))
  (after! which-key
    (which-key-add-keymap-based-replacements +startup-applications-map
                                             "n" "Dunst"
                                             "n ." "Show context menu"
                                             "n C" "Close all notifications"
                                             "n c" "Close notification"
                                             "n h" "History pop"
                                             "n r" "Restart Dunst"
                                             "n x" "Kill Dunst"))

  (if (process-live-p startup/dunst-process)
      (startup/restart-dunst)
    (startup/start-dunst)))
