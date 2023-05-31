;; -*- no-byte-compile: t; -*-
(unpin! t)
(disable-packages! evil-escape
                   flyspell
                   flycheck
                   ccls
                   compat)
(unless (modulep! :config default +smartparens)
  (disable-packages! smartparens))
(when (modulep! :checkers syntax +flymake)
  (disable-packages! flycheck))

;;; UI
(package! info-colors)
(package! authinfo-color-mode :recipe (:host github :repo "tecosaur/authinfo-color-mode"))
(package! goggles)
(package! rainbow-mode)
(package! topsy)

;; cae-theme.el
(package! modus-themes)
(package! circadian)
(package! standard-themes)
(package! ef-themes)

;;; Tools
(package! nov)
(package! syslog-mode)
(package! w3m)
(package! ace-link)
(package! pdftotext :recipe (:host github :repo "tecosaur/pdftotext.el"))

;; These are used for their autoloads without explicit configuration.
(package! crontab-mode)
(package! ebuild-mode :built-in t)
(package! font-lock-studio)             ;I don't use this often, but it's useful
                                        ;for debugging font-lock issues.
(package! git-modes)

;; cae-webkit.el
;; (package! webkit :recipe (:type git :host github :repo "akirakyle/emacs-webkit"
;;                           :branch "main"
;;                           :files (:defaults "*.js" "*.css" "*.so")
;;                           :pre-build ("make")))

;;; Editor
(package! pp+)
(package! avy :recipe (:repo "LemonBreezes/avy" :branch "master"))
(package! winum)
(package! zop-to-char)
(package! aggressive-indent)
(package! hungry-delete)
(package! file-info)
(package! titlecase)
(package! macrursors :recipe (:host github :repo "corytertel/macrursors"))
(when (modulep! :editor multiple-cursors)
  (package! mc-extras))

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
