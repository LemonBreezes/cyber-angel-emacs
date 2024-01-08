;; -*- no-byte-compile: t; -*-
(unpin! t)
(disable-packages! evil-escape
                   flyspell
                   ccls                 ; I use clangd.
                   compat               ; I use the latest version of Emacs.
                   ace-window
                   volatile-highlights
                   hl-line
                   elfeed-goodies
                   mu4e-alert
                   fish-completion)
;; Hopefully Doom will switch to Doom Elpa soon and I can remove this.
(package! code-review :recipe
  (:host github :repo "phelrine/code-review" :branch "fix/closql-update"))
;; Hopefully Hlissner will merge my PR for `evil-org-mode'.
(when (and (modulep! :editor evil +everywhere)
           (modulep! :lang org))
  (package! evil-org :recipe
    (:host github :repo "LemonBreezes/evil-org-mode")))
(package! anzu :recipe
  (:host github :repo "LemonBreezes/anzu" :branch "expose-anzu-update-timer-to-user"))
(package! helpful :recipe
  (:host github :repo "LemonBreezes/helpful" :branch "fix-scan-sexps-error"))

(unless (modulep! :config default +smartparens)
  (disable-packages! smartparens))
(when (modulep! :checkers syntax +flymake)
  (disable-packages! flycheck))

;; cae-evil.el
(when (modulep! :editor evil)
  (package! evil-owl)
  (package! evil-visual-mark-mode))

;; cae-smartparens
(when (and (not (modulep! :editor lispy))
           (modulep! :editor evil))
  (package! evil-cleverparens))

;;; UI
(package! info-colors)
(package! authinfo-color-mode :recipe (:host github :repo "tecosaur/authinfo-color-mode"))
(package! hercules :recipe (:host github :repo "Zetagon/hercules"))
(package! outline-minor-faces)
(package! beacon)
(package! breadcrumb :recipe (:host github :repo "joaotavora/breadcrumb"))
(package! iscroll)
(package! indent-bars :recipe (:host github :repo "jdtsmith/indent-bars"))
(package! nice-citation :recipe (:host github :repo "damiencollard/nice-citation"))
(package! backline)
(package! xterm-color)
(package! fancy-compilation)

;; cae-theme.el
(package! modus-themes)
(package! circadian)
(package! standard-themes)
(package! ef-themes)
(package! crazy-theme :recipe (:host github :repo "eval-exec/crazy-theme.el"))
(package! theme-magic)
(package! ewal)

;;; Tools
(package! nov)
(package! syslog-mode)
(package! pdftotext :recipe (:host github :repo "tecosaur/pdftotext.el"))
(package! wakatime-mode)

;; These are used for their autoloads. They are not explicitly referenced in
;; this configuration.
(package! crontab-mode)
(package! inputrc-mode)
(package! ebuild-mode :built-in t)
(package! font-lock-studio)             ;I don't use this often, but it's useful
                                        ;for debugging font-lock issues.
(package! eff)
(package! git-modes)
(package! 0x0)

;;; Editor
(package! yank-indent :recipe (:host github :repo "jimeh/yank-indent"))
(package! file-info :recipe (:host github :repo "Artawower/file-info.el"))
(package! titlecase)
(package! transpose-frame)
(package! logos)
(package! auto-activating-snippets :recipe (:host github :repo "ymarco/auto-activating-snippets"))
(package! smart-semicolon)
(package! restore-point :recipe (:host github :repo "LemonBreezes/restore-point"))
(package! expand-region-improved :recipe (:host github :repo "leotaku/expand-region-improved"))
(package! auto-sudoedit)
(package! parrot :recipe (:host github :repo "positron-solutions/parrot"))
(package! string-inflection)
(package! beginend)
(package! isearch-mb)
(package! edit-indirect)
(package! string-edit-at-point)
(package! zop-to-char)
(package! i3wm-config-mode)

;; cae-multi.el
(package! git-auto-commit-mode)

;; cae-repeat.el
(package! define-repeat-map :recipe (:repo "https://tildegit.org/acdw/define-repeat-map.el.git"))

;; cae-vlf.el
(package! vlf)

;; cae-visible-scrolling
(package! scrollkeeper)

;; cae-bindings.el
(package! vline)
(package! try)

;;; Autocompletion
(package! consult-yasnippet)
