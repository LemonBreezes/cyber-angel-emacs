;; -*- no-byte-compile: t; -*-
;;; private/misc-applications/packages.el

;; Standalone apps
(package! alarm-clock)
(when (modulep! :app rss)
  (package! elfeed-tube)
  (package! elfeed-tube-mpv))
(package! ement)
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

(and (eq system-type 'gnu/linux) (executable-find "pactl")
     (package! pulseaudio-control))
(when (and (eq system-type 'gnu/linux)
           (not (getenv "WSL_DISTRO_NAME"))
           (or (modulep! :private helm)
               (modulep! :completion helm)))
  (package! helm-linux-disks :recipe
    (:host github :repo "akirak/helm-linux-disks")))
(package! tldr)
(package! speed-type)
(unless (or (memq system-type '(cygwin windows-nt ms-dos))
            (not (or (modulep! :private helm)
                     (modulep! :completion helm))))
  (package! helm-system-packages))
(package! x86-lookup)
(package! devdocs)
(package! trashed)
(package! know-your-http-well)
(package! fireplace)
(package! flames-of-freedom)
(package! snow)
(package! ednc)
(package! autotetris-mode)
(package! paradox)

(package! posimacs-shortdocs :recipe
          (:host github :repo "LemonBreezes/posimacs-shortdocs"))

(package! zones)
(package! zone-nyan)
(package! zone-rainbow)
(package! zone-sl)
(package! zone-matrix :recipe (:host github :repo "ober/zone-matrix"))
(package! zone-tmux-clock)

;; Trash bin
(ignore
 (package! enime :recipe
   (:host github :repo "xl666/enime" :files ("*"))))
