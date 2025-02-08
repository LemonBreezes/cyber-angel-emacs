;;; init.el -*- lexical-binding: t; -*-

;; I added these to help with debugging my config. It's easier to toggle these
;; than to comment out large sections of my config.
(let ((q t))
  (defvar cae-init-preamble-enabled-p (and q t))
  (defvar cae-init-core-enabled-p (and q t))
  (defvar cae-keyboard-remaps-enabled-p (and q nil))
  (let ((p (and q t)))
    (defvar cae-init-ui-enabled-p (and p t))
    (defvar cae-init-tools-enabled-p (and p t))
    (defvar cae-init-editor-enabled-p (and p t))
    (defvar cae-init-autocompletion-enabled-p (and p t))
    (defvar cae-init-text-enabled-p (and p t))
    (defvar cae-init-email-enabled-p (and p t))
    (defvar cae-init-applications-enabled-p (and p t))
    (defvar cae-init-term-enabled-p (and p t))
    (defvar cae-init-languages-enabled-p (and p t))
    (defvar cae-init-appendix-enabled-p (and p t))))

(when cae-init-preamble-enabled-p
  (when (string-equal system-type "android") ;; Add Termux binaries to PATH environment
    (let ((termuxpath "/data/data/com.termux/files/usr/bin"))
      (setenv "PATH" (concat (getenv "PATH") ":" termuxpath))
      (setq exec-path (append exec-path (list termuxpath)))))

  ;; This is so that I don't accidentally start Emacs as a daemon.
  (when (daemonp) (kill-emacs))

  (when (boundp 'safe-local-variable-directories)
    (add-to-list 'safe-local-variable-directories doom-user-dir)
    (add-to-list 'safe-local-variable-directories doom-emacs-dir)
    (add-to-list 'safe-local-variable-directories "~/org")
    (add-to-list 'safe-local-variable-directories (getenv "HOME")))

  ;; Add hostname to the modeline.
  (if (and global-mode-string (listp global-mode-string))
      (appendq! x
                (truncate-string-to-width (secure-hash 'sha512 system-name) 6))
    (setq global-mode-string
          (list (truncate-string-to-width (secure-hash 'sha512 system-name) 6))))

  ;; Set up the secrets directory and its module path.
  (defvar cae-multi-secrets-dir (expand-file-name "secrets/" doom-user-dir))
  (make-directory cae-multi-secrets-dir t)
  (defvar cae-multi-secrets-modules-dir (concat cae-multi-secrets-dir "modules/"))
  (make-directory cae-multi-secrets-modules-dir t)
  (add-to-list 'doom-module-load-path cae-multi-secrets-modules-dir)

  ;; This is where I clone Git projects to.
  (defvar cae-repo-dir "~/src/")

  (when (and (>= (num-processors) 32)
             (not (eq system-type 'windows-nt))
             ;; Experimental incremental garbage collector.
             (not (functionp 'igc-collect))
             ;; 180GB of RAM.
             (> (car (memory-info))
                (* 180 1024 1024)))
    (let ((cae-gc-cons-threshold (* 128 1024 1024 1024)))
      (setq gcmh-high-cons-threshold cae-gc-cons-threshold
            consult--gc-threshold cae-gc-cons-threshold
            cae-hacks-gc-cons-threshold cae-gc-cons-threshold
            +lsp--default-gcmh-high-cons-threshold cae-gc-cons-threshold)))

  ;; Do not start incrementally loading packages until I am actually AFK.
  (setq doom-incremental-first-idle-timer 60)

  (setq native-comp-async-jobs-number (num-processors)
        native-comp-async-report-warnings-errors 'silent)

  (load! "lisp/cae-debug")
  (load! "lisp/cae-hacks")

  ;; Set a fallback theme.
  (setq doom-theme 'wheatgrass)

  (setq evil-undo-system 'undo-fu)

  ;; Make it easier to debug lazy loading issues.
  (when init-file-debug (setq doom-incremental-first-idle-timer nil))

  ;; I never want to see loading messages.
  (unless init-file-debug
    (defadvice! cae-load-ignore-message-a (args) :filter-args #'load
      (cl-destructuring-bind (file &optional noerror nomessage nosuffix must-suffix) args
        (list file noerror t nosuffix must-suffix))))

  (setq doom-leader-alt-key "<menu>"
        doom-localleader-alt-key "<menu> m"))

(doom! :completion
       (vertico +icons +childframe)
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
       fold
       (format +lsp)
       snippets
       multiple-cursors
       (evil +everywhere)
       lispy

       :emacs
       undo
       ibuffer
       vc
       (dired +icons +dirvish)

       :term
       eshell
       (:if (not (memq system-type '(cygwin windows-nt ms-dos))) vterm)

       :checkers
       (syntax +childframe +icons)

       :tools
       (debugger +lsp)
       direnv
       ;;editorconfig ;; Now built into Emacs.
       (eval +overlay)
       (lookup +dictionary +docsets +offline)
       (magit +forge)
       (lsp +peek)
       tree-sitter
       (docker +lsp)
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
       (python +lsp +tree-sitter +pyright +cython)
       (haskell +lsp +tree-sitter)

       :os
       (:if IS-MAC macos)
       (:if (not (memq system-type '(cygwin windows-nt ms-dos))) tty +osc)

       :config
       (default +bindings +smartparens +gnupg)

       :email
       mu4e

       :app
       (rss +org)
       irc

       :cae
       (lean4 +lsp +tree-sitter)
       (helm +childframe)
       debugger
       eshell
       exwm
       dired
       lisp
       misc-applications
       (modeline +emms)
       notifications
       unpackaged
       vc
       org
       (ai +copilot)
       gnus
       rss
       denote

       :secret
       root)

;;Local Variables:
;;eval: (when (featurep 'lispy) (lispy-mode -1))
;;End:
