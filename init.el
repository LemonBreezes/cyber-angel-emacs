;;; init.el -*- lexical-binding: t; -*-

(setq doom-theme 'wheatgrass)           ;Set a dark fallback theme.

(when (version< "30.0" emacs-version)
  (add-to-list 'safe-local-variable-directories doom-user-dir)
  (add-to-list 'safe-local-variable-directories doom-emacs-dir))

(load! "lisp/cae-debug")                ;Debug Emacs.
(load! "lisp/cae-lib")
(load! "lisp/cae-hacks")

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
(defvar cae-keyboard-remaps-enabled-p nil
  "Whether we remap keys for special keyboard layouts.")

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
       (window-select +numbers)
       ophints

       :editor
       file-templates
       format
       snippets
       multiple-cursors
       lispy
       fold
       word-wrap
       (evil +everywhere) ; Be prepared to encounter issues if you use Evil
       ;; with my config. If you encounter any or have suggestions, please raise
       ;; an issue on GitHub.

       :emacs
       undo
       ibuffer
       vc

       :term
       eshell
       (:if (not (memq system-type '(cygwin windows-nt ms-dos))) vterm)

       :checkers
       (syntax +flymake +childframe)
       ;;spell
       ;;grammar

       :tools
       ;; debugger
       direnv
       editorconfig
       (eval +overlay)
       (lookup +dictionary +docsets +offline)
       (magit) ;; +forge is broken for me on the latest Emacs.
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

       :os
       (:if IS-MAC macos)
       (:if (not (memq system-type '(cygwin windows-nt ms-dos))) tty +osc)

       :config
       (default +bindings +smartparens)

       :email
       (mu4e +org)
       ;;(notmuch +org)

       :app
       (rss +org)

       :private
       (corfu +orderless +icons)
       (helm +childframe)
       (debugger +lsp)
       (dirvish +icons +dirvish)
       eshell
       ;; exwm
       dired
       lisp
       misc-applications
       modeline
       unpackaged
       vc
       org
       ai)

;;Local Variables:
;;eval: (when (featurep 'lispy) (lispy-mode -1))
;;End:
