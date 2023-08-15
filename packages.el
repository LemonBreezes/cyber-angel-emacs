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
                   evil-snipe)
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
(unless (modulep! :editor evil)
  (package! goggles))
(package! rainbow-mode)
(package! topsy)
(package! anzu)
(package! isearch-mb)
(package! hercules :recipe (:host github :repo "Zetagon/hercules"))
(package! outline-minor-faces)
(package! beacon)
(package! iscroll)

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
(package! pp+)
(package! zop-to-char)
(package! aggressive-indent)
(package! yank-indent :recipe (:host github :repo "jimeh/yank-indent"))
(package! hungry-delete)
(package! file-info)
(package! titlecase)
(when (modulep! :editor multiple-cursors)
  (package! mc-extras))
(package! speedrect :recipe (:host github :repo "jdtsmith/speedrect"))
(package! avy-embark-collect)
(package! restore-point
  :recipe (:host github :repo "LemonBreezes/restore-point"))
(package! symbol-overlay)
(package! sentex)
(package! edit-indirect)
(package! string-edit-at-point)
(package! expand-region-improved :recipe (:host github :repo "leotaku/expand-region-improved"))
(package! tabgo :recipe (:host github :repo "isamert/tabgo.el"))
(package! multiclip :recipe (:host github :repo "kiennq/highlight2clipboard"))
(package! switch-window)
(package! jinx)
(package! transpose-frame)
(package! logos)
(package! indent-bars :recipe (:host github :repo "jdtsmith/indent-bars"))

;; These are used for their autoloads. They are not explicitly referenced in
;; this configuration.
(package! ialign)

;; cae-multi.el
(package! git-auto-commit-mode)

;; cae-repeat.el
(package! define-repeat-map :recipe (:repo "https://tildegit.org/acdw/define-repeat-map.el.git"))

;; cae-keyboard.el
(package! auto-activating-snippets :recipe (:host github :repo "ymarco/auto-activating-snippets"))
(package! smart-semicolon)

;; cae-vlf.el
(package! vlf)

;; cae-bindings.el
(package! vline)
(package! try)

;;; Autocompletion
(package! isearch-dabbrev)
(package! consult-yasnippet)

;; cae-corfu.el
(when (modulep! :lang org)
  (package! org-block-capf :recipe (:host github :repo "xenodium/org-block-capf")))

;; cae-ido.el
(package! ido-vertical-mode)
(package! flx-ido)

;;; XML
(package! xml-format)
