;; -*- no-byte-compile: t; -*-
;;; cae/misc-applications/packages.el

(package! cae-lib :recipe (:host github :repo "LemonBreezes/cae-lib"))

;; EAF is huge. It gets its own category.
(when (modulep! +eaf)
  (package! eaf :recipe (:host github :repo "emacs-eaf/emacs-application-framework" :files ("*"))))

;; Random apps
(package! alarm-clock)
(package! pomm)
(package! debbugs :recipe (:host github :repo "emacs-straight/debbugs"
                           :files ("*"))) ; Have to get all files to fix error.
(package! leetcode)
(package! noaa)
(package! hackernews)

;; System
(when (eq system-type 'gnu/linux)
  (package! daemons))
(package! disk-usage)
(when (or (modulep! :cae helm)
          (modulep! :completion helm))
  (when (and (eq system-type 'gnu/linux)
             (not (getenv "WSL_DISTRO_NAME")))
    (package! helm-linux-disks :recipe
      (:host github :repo "akirak/helm-linux-disks")))
  (unless (memq system-type '(cygwin windows-nt ms-dos))
    (package! helm-system-packages)))
(and (eq system-type 'gnu/linux) (executable-find "pactl")
     (package! pulseaudio-control))
(package! trashed :recipe (:host github :repo "LemonBreezes/trashed"
                           :branch "cae"))
(package! neato-graph-bar)
(package! journalctl-mode)

;; Insert
(when (or (modulep! :cae helm)
          (modulep! :completion helm))
  (package! helm-rage))
(package! lorem-ipsum)
(package! password-generator)

;; Games
(package! autotetris-mode)
(package! speed-type)
(package! chess)
(package! klondike)
(package! minesweeper)

;; Eye candy
(package! fireplace)
(package! flames-of-freedom :recipe
  (:host github :repo "LemonBreezes/flames-of-freedom"
   :branch "optimize-and-remove-compile-calls"))
(package! snow)
(package! zone-nyan)
(package! zone-rainbow)
(package! zone-sl)
(package! zone-matrix :recipe (:host github :repo "ober/zone-matrix"))
(package! zone-pgm-spoopy :recipe (:host github :repo "twitchy-ears/zone-pgm-spoopy"))
(package! selectric-mode)

;; Music
(package! somafm)
(package! empv)
(package! elfeed-tube) ; For conveniently getting an Invidious instance URL.
(package! emms :recipe (:host github :repo "emacsmirror/emms" :files ("*")))
(package! lyrics-fetcher)
(when (or (modulep! :cae helm)
          (modulep! :completion helm))
  (package! helm-emms))
