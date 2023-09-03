;;; init.el -*- lexical-binding: t; -*-

(add-to-list 'safe-local-variable-directories doom-user-dir)
(add-to-list 'safe-local-variable-directories doom-emacs-dir)

(load! "lisp/cae-debug")
(load! "lisp/cae-lib")
(load! "lisp/cae-hacks")

(add-to-list 'load-path "/usr/share/emacs/site-lisp/ebuild-mode")

;; Set a fallback theme.
(setq doom-theme 'wheatgrass)

(setq native-comp-async-jobs-number (num-processors))

;; I added these to help with debugging my config. It's easier to toggle these
;; than to comment out large sections of my config.
(defvar cae-init-ui-enabled-p t
  "Whether our UI section of `config.el' is enabled.")
(defvar cae-init-tools-enabled-p t
  "Whether our tools section of `config.el' is enabled.")
(defvar cae-init-editor-enabled-p t
  "Whether our editor section of `config.el' is enabled.")
(defvar cae-init-autocompletion-enabled-p t
  "Whether our autocompletion section of `config.el' is enabled.")

(doom! :completion
       (vertico +icons +childframe)

       :ui
       doom-dashboard
       hl-todo
       hydra
       ;;(ligatures +extra +iosevka)
       ;;nav-flash
       (popup +defaults)
       (vc-gutter +pretty +diff-hl)
       vi-tilde-fringe
       workspaces
       window-select
       ophints

       :editor
       file-templates
       format
       snippets
       multiple-cursors
       fold
       (evil +everywhere)
       lispy

       :emacs
       undo
       ibuffer
       vc

       :term
       eshell
       (:if (not (memq system-type '(cygwin windows-nt ms-dos))) vterm)

       :checkers
       (syntax +flymake)
       ;;spell
       ;;grammar

       :tools
       ;; debugger
       direnv
       editorconfig
       (eval +overlay)
       (lookup +dictionary +docsets +offline)
       (magit +forge)
       (lsp +peek +eglot)
       tree-sitter
       pdf

       :lang
       (emacs-lisp +tree-sitter)
       (org +roam2 +tree-sitter +dragndrop)
       (cc +lsp +tree-sitter)
       (web +lsp +tree-sitter)
       (sh +lsp +tree-sitter)
       (nix +lsp +tree-sitter)
       (solidity +lsp +tree-sitter)
       (lua +lsp +tree-sitter +fennel)
       (data +lsp +tree-sitter)

       :os
       (:if IS-MAC macos)
       (:if (not (memq system-type '(cygwin windows-nt ms-dos))) tty +osc)

       :config
       (default +bindings +smartparens)

       :email
       ;;(mu4e +org)
       ;;(notmuch +org)

       :app
       (rss +org)

       :private
       (corfu +orderless +icons +split-char)
       (helm +childframe)
       (debugger +lsp)
       (dirvish +icons +dirvish)
       eshell
       exwm
       dired
       lisp
       misc-applications
       modeline
       unpackaged
       vc
       org
       ai
       gnus
       ;;holy
       )

;;Local Variables:
;;eval: (when (featurep 'lispy) (lispy-mode -1))
;;End:
