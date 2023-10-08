;; -*- no-byte-compile: t; -*-
;;; private/misc-applications/trash/packages.el

(unless (or (memq system-type '(cygwin windows-nt ms-dos))
            (getenv "WSL_DISTRO_NAME"))
  (package! elcord))

;; Lookup
(package! devdocs)
(package! know-your-http-well)
(package! tldr)
(package! x86-lookup)

