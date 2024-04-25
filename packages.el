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
                   mu4e-alert)
;; Hopefully Doom will switch to Doom Elpa soon and I can remove this.
(package! code-review :recipe
  (:host github :repo "phelrine/code-review" :branch "fix/closql-update"))
;; I use the latest version of Org.
(package! org
  :recipe (:host nil
           :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git"
           :local-repo "org"
           :depth full
           :pre-build (straight-recipes-org-elpa--build)
           :build (:not autoloads)
           :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*"))))

;; PRs that I've made but haven't been merged yet.
(when (and (modulep! :editor evil)
           (modulep! :editor multiple-cursors))
  (package! evil-mc :recipe
    (:host github :repo "LemonBreezes/evil-mc" :branch "enable-lexical-binding")))
(when (and (modulep! :editor evil +everywhere)
           (modulep! :lang org))
  (package! evil-org :recipe
    (:host github :repo "LemonBreezes/evil-org-mode")))
(package! helpful :recipe
  (:host github :repo "LemonBreezes/helpful" :branch "fix-scan-sexps-error"))
(package! lsp-mode :recipe
  (:host github :repo "LemonBreezes/lsp-mode" :branch "fix-padding-for-lsp-modeline-progress"))
(package! winum :recipe
  (:host github :repo "LemonBreezes/winum" :branch "fix-install-modeline-function"))

(unless (modulep! :config default +smartparens)
  (disable-packages! smartparens))
(when (modulep! :checkers syntax +flymake)
  (disable-packages! flycheck))

;; cae-evil.el
(when (modulep! :editor evil)
  (package! evil-owl)
  (package! tabgo)
  (package! harpoon))

;; cae-smartparens.el
(when (and (not (modulep! :editor lispy))
           (modulep! :editor evil))
  (package! evil-cleverparens))

;; benchmarks
(package! elisp-benchmarks)

;;; UI
(package! info-colors)
(package! communinfo :recipe (:repo "https://codeberg.org/mekeor/emacs-communinfo.git"))
(package! authinfo-color-mode :recipe (:host github :repo "tecosaur/authinfo-color-mode"))
(package! hercules :recipe (:host github :repo "Zetagon/hercules"))
(package! outline-minor-faces)
(package! beacon)
(package! breadcrumb :recipe (:host github :repo "joaotavora/breadcrumb"))
(package! iscroll)
(package! indent-bars :recipe (:host github :repo "jdtsmith/indent-bars"))
(package! backline)
(package! xterm-color)
(package! fancy-compilation)
(package! casual)

;; cae-theme.el
(package! modus-themes)
(package! circadian)
(package! standard-themes)
(package! ef-themes)
(package! crazy-theme :recipe (:host github :repo "eval-exec/crazy-theme.el"))
(package! theme-magic)
(package! ewal)

;;; Tools
(package! nov :recipe (:host github :repo "emacsmirror/nov"))
(package! pdftotext :recipe (:host github :repo "tecosaur/pdftotext.el"))
(package! keyfreq)
(package! wakatime-mode)
(package! consult-mu :recipe
  (:host github :repo "armindarvish/consult-mu" :files ("*" "extras/*")))
(package! 0x0)

;; cae-lsp.el
(when (modulep! :tools lsp +eglot)
  (package! eglot-booster :recipe (:host github :repo "jdtsmith/eglot-booster")))

;; These are used for their autoloads. They are not explicitly referenced in
;; this configuration.
(package! crontab-mode)
(package! inputrc-mode)
(package! ebuild-mode :built-in t)
(package! tokei)                        ;I use this for counting lines of code in
                                        ;my projects.
(package! font-lock-studio)             ;I don't use this often, but it's useful
                                        ;for debugging font-lock issues.
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
(package! smart-semicolon)
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

;; These are used for their autoloads. They are not explicitly referenced in
;; this configuration.
(package! substitute) ; This is for when `lsp-rename' is not available.

;;; Lang
(when (modulep! :lang haskell)
  (package! consult-hoogle))
