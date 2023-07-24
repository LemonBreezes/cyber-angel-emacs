;; -*- no-byte-compile: t; -*-
;;; private/misc-applications/packages.el

;; Standalone apps
(package! alarm-clock)
(when (modulep! :app rss)
  (package! elfeed-tube)
  (package! elfeed-tube-mpv))
(package! my-repo-pins :recipe
  (:host github :repo "NinjaTrappeur/my-repo-pins"
   :build (:not compile)))

;; Use external APIs or apps
(unless (or (memq system-type '(cygwin windows-nt ms-dos))
            (getenv "WSL_DISTRO_NAME"))
  (package! elcord))
(package! leetcode)
(package! somafm)
(package! wttrin :recipe (:local-repo "packages/wttrin"))

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
(package! paradox)
(and (eq system-type 'gnu/linux) (executable-find "pactl")
     (package! pulseaudio-control))
(package! trashed)

;; Lookup
(package! devdocs)
(package! know-your-http-well)
(package! tldr)
(package! x86-lookup)
(package! posimacs-shortdocs :recipe
  (:host github :repo "LemonBreezes/posimacs-shortdocs"))

;; Games
(package! autotetris-mode)
(package! speed-type)

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

;; Emacs OS
(package! ednc)

;; Trash bin
(ignore
 (package! enime :recipe
   (:host github :repo "xl666/enime" :files ("*"))))
