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
                  (which-key-show-keymap 'pulseaudio-control-map t))))))
  :config
  (after! which-key
    (push '((nil . "pulseaudio-control-\\(.*\\)") . (nil . "\\1"))
          which-key-replacement-alist))
  (setq pulseaudio-control-use-default-sink t)
  (pulseaudio-control-default-keybindings))
