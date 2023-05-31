;; -*- no-byte-compile: t; -*-
(unpin! t)
(disable-packages! evil-escape
                   flyspell
                   ccls)
(unless (modulep! :config defaulnt +smartparens)
  (disable-packages! smartparens))

;;; UI

(package! info-colors)
(package! authinfo-color-mode :recipe (:host github :repo "tecosaur/authinfo-color-mode"))
(package! goggles)

;; cae-theme.el
(package! modus-themes)
(package! circadian)

(package! standard-themes)
(package! ef-themes)

;;; Tools

(package! nov)
(package! syslog-mode)

(package! crontab-mode)
(package! ebuild-mode :built-in t)
(package! font-lock-studio)

;; cae-webkit.el
(package! webkit :recipe (:type git :host github :repo "akirakyle/emacs-webkit"
                          :branch "main"
                          :files (:defaults "*.js" "*.css" "*.so")
                          :pre-build ("make")))

;;; Editor

(package! pp+)
(package! avy :recipe (:repo "LemonBreezes/avy" :branch "master"))
(package! zop-to-char)

;; cae-multi.el
(package! git-auto-commit-mode)
(package! ts)

;; cae-repeat.el
(package! define-repeat-map :recipe (:repo "https://tildegit.org/acdw/define-repeat-map.el.git"))

;; cae-keyboard.el
(package! home-row-numbers :recipe (:host github :repo "LemonBreezes/home-row-numbers"))

;;; Autocompletion

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
(package! isearch-dabbrev)
(package! consult-yasnippet)
