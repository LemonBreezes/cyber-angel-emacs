;;; vanilla-emacs-configs/projectile-tramp-leaking-roots.el -*- lexical-binding: t; -*-

;; Bootstrap straight
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                                    'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'tramp-sh)
(straight-use-package 'projectile)
(projectile-mode +1)
(projectile-discover-projects-in-directory "~/src")

(find-file "~/src/emacs/README")
(find-file "/sudo:root@localhost:/usr/share/emacs/31.0.50/src/keyboard.c")
(find-file "~/src/doomemacs/README.md")
(switch-to-buffer (get-file-buffer "~/src/emacs/README"))
(message "current project root: %s" (projectile-project-root))
