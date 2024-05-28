;;; init.el -*- lexical-binding: t; -*-

(when (string-equal system-type "android")  ;; Add Termux binaries to PATH environment
  (let ((termuxpath "/data/data/com.termux/files/usr/bin"))
    (setenv "PATH" (concat (getenv "PATH") ":" termuxpath))
    (setq exec-path (append exec-path (list termuxpath)))))

;; This is so that I don't accidentally start Emacs as a daemon.
(when (daemonp) (kill-emacs))

(when (boundp 'safe-local-variable-directories)
  (add-to-list 'safe-local-variable-directories doom-user-dir)
  (add-to-list 'safe-local-variable-directories doom-emacs-dir)
  (add-to-list 'safe-local-variable-directories (getenv "HOME")))

(when (and (>= (num-processors) 32)
           (> (car (memory-info))
              (* 180 1024 1024)))
  (let ((cae-gc-cons-threshold (* 128 1024 1024 1024)))
    (setq gcmh-high-cons-threshold cae-gc-cons-threshold
          consult--gc-threshold cae-gc-cons-threshold
          cae-hacks-gc-cons-threshold cae-gc-cons-threshold
          +lsp--default-gcmh-high-cons-threshold cae-gc-cons-threshold)))
(setq doom-incremental-idle-timer 0.25)

(setq native-comp-async-jobs-number (num-processors)
      native-comp-async-report-warnings-errors 'silent)

(load! "lisp/cae-debug")
(load! "lisp/cae-lib")
(load! "lisp/cae-hacks")

;; Set a fallback theme.
(setq doom-theme 'wheatgrass)

;; I added these to help with debugging my config. It's easier to toggle these
;; than to comment out large sections of my config.
(defvar cae-init-core-enabled-p t)
(defvar cae-keyboard-remaps-enabled-p t)
(let ((p t))
  (defvar cae-init-ui-enabled-p (and p t))
  (defvar cae-init-tools-enabled-p (and p t))
  (defvar cae-init-editor-enabled-p (and p t))
  (defvar cae-init-autocompletion-enabled-p (and p t))
  (defvar cae-init-text-enabled-p (and p t))
  (defvar cae-init-email-enabled-p (and p t))
  (defvar cae-init-term-enabled-p (and p t))
  (defvar cae-init-languages-enabled-p (and p t)))

(setq evil-undo-system 'undo-fu)

;; Make it easier to debug lazy loading issues.
(when init-file-debug (setq doom-incremental-first-idle-timer nil))

;; I never want to see loading messages.
(unless init-file-debug
  (defadvice! cae-load-ignore-message-a (args) :filter-args #'load
    (cl-destructuring-bind (file &optional noerror nomessage nosuffix must-suffix) args
      (list file noerror t nosuffix must-suffix))))

(setq doom-leader-alt-key "<menu>"
      doom-localleader-alt-key "<menu> m")

(doom! :completion
       (vertico +icons)
       (corfu +orderless +icons +dabbrev)

       :ui
       hl-todo
       (hydra +childframe)
       (popup +defaults)
       (vc-gutter +pretty +diff-hl)
       vi-tilde-fringe
       workspaces
       (window-select +numbers)
       ophints
       (treemacs +lsp)

       :editor
       file-templates
       format
       snippets
       multiple-cursors
       (evil +everywhere)

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
       pdf
       biblio

       :lang
       (emacs-lisp +tree-sitter)
       (org +roam2 +tree-sitter +dragndrop +journal)
       (cc +lsp +tree-sitter)
       (web +lsp +tree-sitter)
       (sh +lsp +tree-sitter)
       (nix +lsp +tree-sitter)
       (lua +lsp +tree-sitter +fennel)
       (data +lsp +tree-sitter)
       (latex +lsp +tree-sitter +cdlatex +latexmk +fold)
       (json +lsp +tree-sitter)
       (python +lsp +tree-sitter)
       (haskell +lsp +tree-sitter)
       (idris +lsp +tree-sitter)

       :os
       (:if IS-MAC macos)
       (:if (not (memq system-type '(cygwin windows-nt ms-dos))) tty +osc)

       :config
       (default +bindings +smartparens)

       :email
       mu4e

       :app
       (rss +org)

       :cae
       ;; helm ; Broken on Emacs 30.
       debugger
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
       rss
       denote)

;;Local Variables:
;;eval: (when (featurep 'lispy) (lispy-mode -1))
;;End:
