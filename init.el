;;; init.el -*- lexical-binding: t; -*-

;; This is so that I don't accidentally start Emacs as a daemon.
(when (daemonp) (kill-emacs))

(add-to-list 'safe-local-variable-directories doom-user-dir)
(add-to-list 'safe-local-variable-directories doom-emacs-dir)

(let ((noninteractive-shell (or (executable-find "dash")
                                (executable-find "sh")))
      (interactive-shell (or (executable-find "zsh")
                             (executable-find "bash"))))
  (setq shell-file-name noninteractive-shell)
  (setenv "SHELL" noninteractive-shell)
  (after! vterm
    (setq vterm-shell interactive-shell)))

(when (>= (num-processors) 32)
  ;; Testing disabling GC during work. Careful with this. I found this was not
  ;; worth it on my i9 9900K but maybe with a 7950X I can get away with it.
  (let ((cae-gc-cons-threshold (* 64 1024 1024 1024)))
    (setq gcmh-high-cons-threshold cae-gc-cons-threshold
          consult--gc-threshold cae-gc-cons-threshold
          cae-hacks-gc-cons-threshold cae-gc-cons-threshold
          +lsp--default-gcmh-high-cons-threshold cae-gc-cons-threshold)))

(setq native-comp-async-jobs-number (num-processors)
      native-comp-async-report-warnings-errors 'silent)

(load! "lisp/cae-debug")
(load! "lisp/cae-lib")
(load! "lisp/cae-hacks")

;; Set a fallback theme.
(setq doom-theme 'wheatgrass)

;; I added these to help with debugging my config. It's easier to toggle these
;; than to comment out large sections of my config.
(let ((p t))
  (defvar cae-init-ui-enabled-p (and p t))
  (defvar cae-init-tools-enabled-p (and p t))
  (defvar cae-init-editor-enabled-p (and p t))
  (defvar cae-init-autocompletion-enabled-p (and p t))
  (defvar cae-init-text-enabled-p (and p t))
  (defvar cae-init-email-enabled-p (and p t))
  (defvar cae-init-term-enabled-p (and p t)))

(setq evil-undo-system 'undo-fu)

;; Make it easier to debug lazy loading issues.
(when init-file-debug (setq doom-incremental-first-idle-timer nil))

(doom! :completion
       (vertico +icons)
       (corfu +tng +orderless +icons +dabbrev +dict +emoji)

       :ui
       doom-dashboard
       hl-todo
       (hydra +childframe)
       ;;(ligatures +extra +iosevka)
       ;;nav-flash
       (popup +defaults)
       (vc-gutter +pretty +diff-hl)
       vi-tilde-fringe
       workspaces
       (window-select +numbers)
       ophints
       zen
       (treemacs +lsp)

       :editor
       file-templates
       format
       snippets
       multiple-cursors
       ;;fold
       (evil +everywhere)
       lispy

       :emacs
       undo
       ibuffer
       vc
       (dired +icons)

       :term
       eshell
       (:if (not (memq system-type '(cygwin windows-nt ms-dos))) vterm)

       :checkers
       (syntax +childframe)
       ;;spell
       ;;grammar

       :tools
       (debugger +lsp)
       direnv
       editorconfig
       (eval +overlay)
       (lookup +dictionary +docsets +offline)
       (magit +forge)
       (lsp +peek)
       tree-sitter
       docker
       ein
       pdf

       :lang
       (emacs-lisp +tree-sitter)
       (org +roam2 +tree-sitter +dragndrop)
       (cc +lsp +tree-sitter)
       (web +lsp +tree-sitter)
       (sh +lsp +tree-sitter)
       (nix +lsp +tree-sitter)
       ;;(solidity +lsp +tree-sitter)
       (lua +lsp +tree-sitter +fennel)
       (data +lsp +tree-sitter)
       (latex +lsp +tree-sitter +cdlatex +latexmk +fold)
       ;;(yaml +lsp +tree-sitter)
       (json +lsp +tree-sitter)
       ;;(javascript +lsp +tree-sitter)
       ;;(python +lsp +tree-sitter)
       ;;(haskell +lsp +tree-sitter)
       ;;(agda +lsp +tree-sitter +local)

       :os
       (:if IS-MAC macos)
       (:if (not (memq system-type '(cygwin windows-nt ms-dos))) tty +osc)

       :config
       (default +bindings +smartparens)

       :email
       mu4e
       ;;(notmuch +org)

       :app
       (rss +org)

       :cae
       ;;holy
       helm
       debugger
       ;;(dirvish +icons +dirvish)
       eshell
       exwm
       dired
       lisp
       misc-applications
       (modeline +wakatime)
       unpackaged
       vc
       org
       ai
       gnus
       eww)

;;Local Variables:
;;eval: (when (featurep 'lispy) (lispy-mode -1))
;;End:
