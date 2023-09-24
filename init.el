;;; init.el -*- lexical-binding: t; -*-

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

(load! "lisp/cae-debug")
(load! "lisp/cae-lib")
(load! "lisp/cae-hacks")

;; Set a fallback theme.
(setq doom-theme 'wheatgrass)

(setq native-comp-async-jobs-number (num-processors))

;; I added these to help with debugging my config. It's easier to toggle these
;; than to comment out large sections of my config.
(defvar cae-init-ui-enabled-p nil
  "Whether our UI section of `config.el' is enabled.")
(defvar cae-init-tools-enabled-p nil
  "Whether our tools section of `config.el' is enabled.")
(defvar cae-init-editor-enabled-p nil
  "Whether our editor section of `config.el' is enabled.")
(defvar cae-init-autocompletion-enabled-p nil
  "Whether our autocompletion section of `config.el' is enabled.")
(defvar cae-init-text-enabled-p nil
  "Whether our text section of `config.el' is enabled.")
(defvar cae-init-email-enabled-p nil
  "Whether our email section of `config.el' is enabled.")
(defvar cae-init-term-enabled-p nil
  "Whether our term section of `config.el' is enabled.")

(setq evil-undo-system 'undo-fu
      evil-want-C-u-scroll t
      evil-want-C-u-delete t)

(doom! :completion
       (vertico +icons +childframe)
       (corfu +icons +orderless +split-char)

       :ui
       doom-dashboard
       hl-todo
       hydra
       (ligatures +extra +iosevka)
       ;;nav-flash
       (popup +defaults)
       (vc-gutter +pretty +diff-hl)
       vi-tilde-fringe
       workspaces
       (window-select +numbers)
       ophints
       treemacs
       ;;unicode
       ;;zen

       :editor
       file-templates
       format
       snippets
       multiple-cursors
       fold
       (evil +everywhere)
       ;;lispy

       :emacs
       undo
       ibuffer
       vc
       (dired +icons)

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
       docker
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
                                        ; (latex +lsp +tree-sitter +cdlatex +latexmk +fold)

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
       ;;holy
       (helm +childframe)
       (debugger +lsp)
       ;;(dirvish +icons +dirvish)
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
       )

;;Local Variables:
;;eval: (when (featurep 'lispy) (lispy-mode -1))
;;End:
