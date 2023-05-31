;;; private/misc-applications/pulseaudio-control.el -*- lexical-binding: t; -*-

(use-package! pulseaudio-control
  :defer t
  :when (and (eq system-type 'gnu/linux)
             (executable-find "pactl"(and (eq system-type 'gnu/linux)
                                          (executable-find "pactl"))))
  :init
  (map! :map ctl-x-map
        "/" (cmd!
             ;; Lazy load `pulseaudio-control'.
             (pulseaudio-control-default-keybindings)
             (setq unread-command-events (list ?\C-x ?/))
             (setq which-key-inhibit t)
             (add-transient-hook! 'pre-command-hook
               (setq which-key-inhibit nil))
             (run-with-idle-timer
              which-key-idle-delay nil
              (lambda ()
                (when which-key-inhibit
                  (which-key-show-keymap
                   'pulseaudio-control-map))))))
  :config
  (setq pulseaudio-control-use-default-sink t
        pulseaudio-control-volume-step "10%")
  (pulseaudio-control-default-keybindings)
  (map! :map pulseaudio-control-map
        "_" #'pulseaudio-control-decrease-volume
        "+" #'pulseaudio-control-increase-volume
        "=" #'pulseaudio-control-increase-volume-less
        "-" #'pulseaudio-control-decrease-volume-less))
