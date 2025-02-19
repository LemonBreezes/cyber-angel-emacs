;; -*- no-byte-compile: t; -*-
(disable-packages! flyspell
                   ccls                 ; I use clangd.
                   compat               ; I use the latest version of Emacs.
                   ace-window
                   volatile-highlights
                   hl-line
                   elfeed-goodies
                   mu4e-alert)
(unpin! consult) ; (void-function consult--process-collection) in consult-gh
(unpin! helpful) ; Keep it up-to-date for bug fixes.
(unpin! :lang :tools :emacs :email :input :term :checkers :ui :editor)
(unpin! corfu)
(unpin! smartparens)
(package! which-key :built-in t)

;; PRs that haven't been merged yet.
(when (and (modulep! :editor evil +everywhere)
           (modulep! :lang org))
  (package! evil-org :recipe
    (:host github :repo "LemonBreezes/evil-org-mode")
    :pin "4d30406efc7c945069c217a051114cc4ad3e7d5a"))
(when (modulep! :editor lispy)
  (package! lispy :recipe (:host github :repo "enzuru/lispy")))

(package! mu4e :built-in t)

(unless (modulep! :config default +smartparens)
  (disable-packages! smartparens))
(when (modulep! :checkers syntax +flymake)
  (disable-packages! flycheck))

;; Preamble
(package! cae-lib :recipe (:host github :repo "LemonBreezes/cae-lib"))
(package! persistent-scratch)

;; cae-evil.el
(when (modulep! :editor evil)
  (package! evil-owl)
  (package! tabgo)
  (package! evil-tutor)
  (package! key-chord :recipe (:host github :repo "LemonBreezes/key-chord")))

;; cae-smartparens.el
(when (and (not (modulep! :editor lispy))
           (modulep! :editor evil))
  (package! evil-cleverparens))

;; benchmarks
(package! elisp-benchmarks)

;;; UI
(package! info-colors)
(package! communinfo)
(package! authinfo-color-mode :recipe (:host github :repo "tecosaur/authinfo-color-mode"))
(package! hercules :recipe (:host github :repo "Zetagon/hercules"))
(package! beacon)
(package! iscroll)
(package! indent-bars :recipe (:host github :repo "jdtsmith/indent-bars"))
(package! backline)
(package! xterm-color)
(package! casual-lib :recipe (:host github :repo "kickingvegas/casual-lib"))
(package! casual-calc :recipe (:host github :repo "kickingvegas/casual-calc"))
(package! syslog-mode)

;; cae-theme.el
(package! modus-themes)
(package! circadian :recipe (:host github :repo "guidoschmidt/circadian.el"))
(package! standard-themes)
(package! ef-themes)
(package! crazy-theme :recipe (:host github :repo "eval-exec/crazy-theme.el"))
(package! girly-notebook-theme :recipe (:host github :repo "melissaboiko/girly-notebook-theme"))
(package! theme-magic)
(package! ewal)

;;; Tools
(package! nov :recipe (:host github :repo "emacsmirror/nov"))
(package! w3m)
(package! pdftotext :recipe (:host github :repo "tecosaur/pdftotext.el"))
(package! keyfreq)
(when (modulep! :email mu4e)
  (package! consult-mu :recipe
    (:host github :repo "armindarvish/consult-mu" :files ("*" "extras/*"))))
(package! 0x0)
(package! detached :recipe (:host sourcehut :repo "niklaseklund/detached.el" :branch "lean"))

;; cae-lsp.el
(when (modulep! :tools lsp +eglot)
  (package! eglot-booster :recipe (:host github :repo "jdtsmith/eglot-booster"))
  (package! breadcrumb :recipe (:host github :repo "joaotavora/breadcrumb")))

;; These are used for their autoloads. They are not explicitly referenced in
;; this configuration.
(package! crontab-mode)
(package! inputrc-mode)
(package! ebuild-mode :built-in t)
(package! portage-modes :recipe (:host github :repo "OpenSauce04/portage-modes"))
(package! systemd)
(package! tokei)                        ;I use this for counting lines of code
                                        ;in my projects.
(package! font-lock-studio)             ;I don't use this often, but it's useful
                                        ;for debugging font-lock issues.
(package! show-font :recipe ;Preview fonts
  (:host github :repo "protesilaos/show-font"))
(package! eff)
(package! git-modes)
(package! huff-mode :recipe (:host github :repo "GokhanPolat/emacs-huff-mode"))

;;; Editor
(package! yank-indent :recipe (:host github :repo "jimeh/yank-indent"))
(package! file-info :recipe (:host github :repo "Artawower/file-info.el"))
(package! titlecase)
(package! transpose-frame)
(package! logos)
(package! auto-activating-snippets :recipe (:host github :repo "ymarco/auto-activating-snippets"))
(package! restore-point :recipe (:host github :repo "arthurcgusmao/restore-point"))
(package! expand-region-improved :recipe (:host github :repo "leotaku/expand-region-improved"))
(package! parrot :recipe (:host github :repo "positron-solutions/parrot"))
(package! string-inflection)
(package! beginend)
(package! isearch-mb)
(package! edit-indirect)
(package! string-edit-at-point)
(package! zop-to-char)
(package! i3wm-config-mode)
(package! auto-sudoedit)

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
(package! ov)
(package! substitute)

;;; Lang
(when (modulep! :lang haskell)
  (package! consult-hoogle))

;;; Term
(package! comint-histories)
