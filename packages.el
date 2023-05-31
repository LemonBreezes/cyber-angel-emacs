;; -*- no-byte-compile: t; -*-
(unpin! t)
(disable-packages! avy
                   smartparens
                   evil-escape
                   projectile)

;;; UI

(package! info-colors)
(package! authinfo-color-mode :recipe (:host github :repo "tecosaur/authinfo-color-mode"))
(package! topsy)
(package! org-sticky-header)

;; cae-theme.el
(package! modus-themes)
(package! circadian)

;;; Editor

(package! pp+)

;;; Autocompletion

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
