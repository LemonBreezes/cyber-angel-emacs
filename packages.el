;; -*- no-byte-compile: t; -*-
(unpin! t)
(disable-packages! evil-escape
                   flyspell
                   ccls                 ; I use clangd.
                   compat               ; I use the latest version of Emacs.
                   mu4e-alert           ; Mu4e comes with `mu4e-modeline-mode'
                                        ; and I don't need email notifications.
                   ace-window
                   rainbow-delimiters
                   volatile-highlights
                   evil-snipe
                   hl-line
                   flymake-popon)
(when (modulep! editor evil)
  (package! evil-embrace :recipe
    (:host github :repo "LemonBreezes/evil-embrace"
     :branch "fix-for-latest-evil-surround")))

(unless (modulep! :config default +smartparens)
  (disable-packages! smartparens))
(when (modulep! :checkers syntax +flymake)
  (disable-packages! flycheck))
;; Added because of errors.
(package! lv)

;; cae-evil.el
(package! evil-god-state)

;;; UI
(package! info-colors)
(package! authinfo-color-mode :recipe (:host github :repo "tecosaur/authinfo-color-mode"))
(package! rainbow-mode)
(package! topsy)
(package! hercules :recipe (:host github :repo "Zetagon/hercules"))
(package! outline-minor-faces)
(package! beacon)
(package! iscroll)
(package! indent-bars :recipe (:host github :repo "jdtsmith/indent-bars"))

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
(package! dwim-shell-command)
(package! posimacs-shortdocs :recipe
  (:host github :repo "LemonBreezes/posimacs-shortdocs"))
(package! wakatime-mode)

;; These are used for their autoloads. They are not explicitly referenced in
;; this configuration.
(package! crontab-mode)
(package! ebuild-mode :built-in t)
(package! font-lock-studio)             ;I don't use this often, but it's useful
                                        ;for debugging font-lock issues.
(package! git-modes)
(package! free-keys)
(package! 0x0)

;;; Editor
(package! yank-indent :recipe (:host github :repo "jimeh/yank-indent"))
(package! file-info)
(package! titlecase)
(package! avy-embark-collect)
(package! restore-point
  :recipe (:host github :repo "LemonBreezes/restore-point"))
(package! sentex)
(package! transpose-frame)
(package! logos)
(package! auto-activating-snippets :recipe (:host github :repo "ymarco/auto-activating-snippets"))
(package! smart-semicolon)

;; These are used for their autoloads. They are not explicitly referenced in
;; this configuration.
(when (not (modulep! :editor evil))
  (package! ialign))

;; cae-multi.el
(package! git-auto-commit-mode)

;; cae-repeat.el
(package! define-repeat-map :recipe (:repo "https://tildegit.org/acdw/define-repeat-map.el.git"))

;; cae-vlf.el
(package! vlf)

;; cae-bindings.el
(package! vline)
(package! try)

;;; Autocompletion
(package! consult-yasnippet)

;; cae-corfu.el
(when (modulep! :lang org)
  (package! org-block-capf :recipe (:host github :repo "xenodium/org-block-capf")))

;; cae-ido.el
(package! ido-vertical-mode)
(package! flx-ido)

;;; XML
(package! xml-format)
