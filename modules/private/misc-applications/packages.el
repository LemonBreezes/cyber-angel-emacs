;; -*- no-byte-compile: t; -*-
;;; private/misc-applications/packages.el

;; This is where my cheatsheets are.
(package! cae-cheatsheets :recipe (:host github :repo "LemonBreezes/cae-cheatsheets"))

;; Standalone apps
(package! alarm-clock)
(when (modulep! :app rss)
  (package! elfeed-tube)
  (package! elfeed-tube-mpv))

;; Use external APIs or apps
(package! leetcode)
(when (modulep! :completion vertico)
  (package! consult-gh :recipe (:host github :repo "armindarvish/consult-gh" :branch "main")))

;; System
(when (eq system-type 'gnu/linux)
  (package! daemons))
(package! disk-usage)
(when (or (modulep! :private helm)
          (modulep! :completion helm))
  (when (and (eq system-type 'gnu/linux)
             (not (getenv "WSL_DISTRO_NAME")))
    (package! helm-linux-disks :recipe
      (:host github :repo "akirak/helm-linux-disks")))
  (unless (memq system-type '(cygwin windows-nt ms-dos))
    (package! helm-system-packages)))
(and (eq system-type 'gnu/linux) (executable-find "pactl")
     (package! pulseaudio-control))
(package! trashed)
(package! neato-graph-bar)

;; Insert
(when (or (modulep! :private helm)
          (modulep! :completion helm))
  (package! helm-rage))
(package! lorem-ipsum)
(package! password-generator)

;; Games
(package! autotetris-mode)
(package! speed-type)
(package! klondike)                     ; TODO

;; Eye candy
(package! fireplace)
(package! flames-of-freedom)
(package! snow)
(package! zones)
(package! zone-nyan)
(package! zone-rainbow)
(package! zone-sl)
(package! zone-matrix :recipe (:host github :repo "ober/zone-matrix"))
(package! zone-tmux-clock)

;; Music
(package! somafm)
(package! empv)
(package! emms)
(when (or (modulep! :private helm)
          (modulep! :completion helm))
  (package! helm-emms))
