;;; private/exwm/startup-programs/trash/vmtouch.el -*- lexical-binding: t; -*-

(when (> (car (memory-info)) (* 63 1024 1024))
  (eval `(start-process "vmtouch" startup/misc-shell-commands-buffer "vmtouch" "-dl"
          doom-user-dir doom-emacs-dir "/usr/share/emacs"
          ,@native-comp-eln-load-path
          ,@(list (executable-find "emacs")
                  (executable-find "emacsclient")
                  startup/discord-executable
                  startup/chromium-executable
                  "~/.cache"))
        t))
