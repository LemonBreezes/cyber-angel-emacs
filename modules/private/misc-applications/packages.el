;; -*- no-byte-compile: t; -*-
;;; private/misc-applications/packages.el

;; Standalone apps
(package! alarm-clock)
(package! elfeed-tube)
(package! elfeed-tube-mpv)


(and (eq system-type 'gnu/linux) (executable-find "pactl")
     (package! pulseaudio-control))
(package! somafm)
(package! enime :recipe
  (:host github :repo "xl666/enime" :files ("*")))
(when (and (eq system-type 'gnu/linux)
           (not (getenv "WSL_DISTRO_NAME"))
           (or (modulep! :private helm)
               (modulep! :completion helm)))
  (package! helm-linux-disks :recipe
    (:host github :repo "akirak/helm-linux-disks")))
(package! tldr)
(package! speed-type)
(package! disk-usage)
(unless (or (memq system-type '(cygwin windows-nt ms-dos))
            (getenv "WSL_DISTRO_NAME"))
  (package! elcord))
(when (eq system-type 'gnu/linux)
  (package! daemons))
(unless (or (memq system-type '(cygwin windows-nt ms-dos))
            (not (or (modulep! :private helm)
                     (modulep! :completion helm))))
  (package! helm-system-packages))
(package! x86-lookup)
(package! devdocs)
(package! trashed)
(package! know-your-http-well)
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
(package! paradox)
(package! ement)

(package! posimacs-shortdocs :recipe
          (:host github :repo "LemonBreezes/posimacs-shortdocs"))

(package! zones)
(package! zone-nyan)
(package! zone-rainbow)
(package! zone-sl)
(package! zone-matrix :recipe (:host github :repo "ober/zone-matrix"))
(package! zone-tmux-clock)
