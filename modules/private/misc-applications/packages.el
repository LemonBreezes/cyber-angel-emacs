;; -*- no-byte-compile: t; -*-
;;; private/misc-applications/packages.el

(and (eq system-type 'gnu/linux) (executable-find "pactl")
     (package! pulseaudio-control))
(package! vuiet)
(package! somafm)
(package! enime :recipe (:host github :repo "xl666/enime" :files ("*")))
(package! alarm-clock)
(when (eq system-type 'gnu/linux)
  (package! helm-linux-disks :recipe (:host github :repo "akirak/helm-linux-disks")))
(package! mentor)
(package! forecast)
(package! tldr)
(package! speed-type)
(package! disk-usage)
(unless (memq system-type '(cygwin windows-nt ms-dos))
  (package! elcord))
(package! picpocket)
(when (eq system-type 'gnu/linux)
  (package! daemons))
(when (and (eq system-type 'gnu/linux) (executable-find "pacman"))
  (package! aurel))
(unless (memq system-type '(cygwin windows-nt ms-dos))
  (package! helm-system-packages))
(package! ytel)
(package! x86-lookup)
(package! devdocs)
(package! trashed)
(package! frameshot)
(package! webpaste)
(package! mu4e :built-in t)
(package! try)
(package! know-your-http-well)
(package! exercism-modern
  :recipe (:host github :repo "elken/exercism-modern" :files ("*/*" "*")))
(package! my-repo-pins :recipe
  (:host github :repo "NinjaTrappeur/my-repo-pins"
   :build (:not compile)))

(package! elfeed-tube)
(package! elfeed-tube-mpv)
(package! ace-link)
