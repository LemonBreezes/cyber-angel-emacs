;; -*- no-byte-compile: t; -*-
;;; private/misc-applications/packages.el

(and (eq system-type 'gnu/linux) (executable-find "pactl")
     (package! pulseaudio-control))
;;(package! vuiet)
(package! somafm)
(package! enime :recipe
  (:host github :repo "xl666/enime" :files ("*")))
(package! alarm-clock)
(when (and (eq system-type 'gnu/linux)
           (not (getenv "WSL_DISTRO_NAME"))
           (or (modulep! :private helm)
               (modulep! :completion helm)))
  (package! helm-linux-disks :recipe
    (:host github :repo "akirak/helm-linux-disks")))
;; (package! mentor)
(package! tldr)
(package! speed-type)
(package! disk-usage)
(unless (or (memq system-type '(cygwin windows-nt ms-dos))
            (getenv "WSL_DISTRO_NAME"))
  (package! elcord))
;; (package! picpocket)
(when (eq system-type 'gnu/linux)
  (package! daemons))
;;(when (and (eq system-type 'gnu/linux) (executable-find "pacman"))
;;  (package! aurel))
(unless (or (memq system-type '(cygwin windows-nt ms-dos))
            (not (or (modulep! :private helm)
                     (modulep! :completion helm))))
  (package! helm-system-packages))
(package! x86-lookup)
(package! devdocs)
(package! trashed)
;; (package! frameshot)
(package! webpaste)
(package! try)
(package! know-your-http-well)
;;(package! exercism-modern :recipe
;;  (:host github :repo "elken/exercism-modern" :files ("*/*" "*")))
(package! my-repo-pins :recipe
  (:host github :repo "NinjaTrappeur/my-repo-pins"
   :build (:not compile)))
(package! fireplace)
(package! flames-of-freedom)
(package! snow)
(package! ednc)
(package! wttrin :recipe (:local-repo "packages/wttrin"))
(package! leetcode)
(package! autotetris-mode)
(package! ytel)

(package! posimacs-shortdocs :recipe
  (:host github :repo "LemonBreezes/posimacs-shortdocs"))

(package! zones)
(package! zone-nyan)
(package! zone-rainbow)
(package! zone-sl)
(package! zone-matrix :recipe (:host github :repo "ober/zone-matrix"))

(package! elfeed-tube)
(package! elfeed-tube-mpv)
