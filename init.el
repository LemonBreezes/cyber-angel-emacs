;;; init.el -*- lexical-binding: t; -*-

;; Set up constants
(defconst cae-multi-secrets-dir (concat doom-user-dir "secrets/"))
(defconst cae-multi-local-dir (concat cae-multi-secrets-dir "shared-local/"))
(defconst cae-multi-data-dir (concat cae-multi-local-dir "etc/"))
(defconst cae-multi-cache-dir (concat cae-multi-local-dir "cache/"))
(defconst cae-multi-org-dir (concat cae-multi-local-dir "org/"))
(defconst cae-multi-secrets-modules-dir (concat cae-multi-secrets-dir "modules/"))
(defconst cae-repo-dir (expand-file-name "~/src/"))
(make-directory cae-multi-local-dir t)
(make-directory cae-multi-data-dir t)
(make-directory cae-multi-cache-dir t)
(make-directory cae-multi-org-dir t)
(make-directory cae-multi-secrets-modules-dir t)
(make-directory (concat doom-user-dir "snippets") t)
(add-to-list 'doom-module-load-path cae-multi-secrets-modules-dir)
(unless (featurep 'cae-lib)
  (defalias 'cae-defadvice! 'defadvice!)
  (defalias 'cae-advice-add 'advice-add))

;; I added these to help with debugging my config. It's easier to toggle these
;; than to comment out large sections of my config.
(let ((q t))
  (let ((p (and q t)))
    (defvar cae-init-preamble-enabled-p (and p t))
    (defvar cae-init-debug-enabled-p (and p t))
    (defvar cae-init-dir-locals-enabled-p (and p t))
    (defvar cae-init-hacks-enabled-p (and p t)))
  (let ((c (and q t)))
    (defvar cae-init-core-enabled-p (and c t))
    (defvar cae-init-tty-enabled-p (and c t))
    (defvar cae-init-bindings-enabled-p (and c t))
    (defvar cae-init-multi-enabled-p (and c t))
    (defvar cae-init-smartparens-enabled-p (and c t))
    (defvar cae-init-projectile-enabled-p (and c t))
    (defvar cae-init-evil-enabled-p (and c t))
    (defvar cae-init-exwm-enabled-p (and c t)))
  (defvar cae-init-keyboard-remaps-enabled-p (and q nil))
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
  (load! "preamble" doom-user-dir))

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

       :editor
       file-templates
       fold
       (format +lsp)
       snippets
       multiple-cursors
       (evil +everywhere)
       ;;lispy

       :emacs
       undo
       ibuffer
       vc
       (dired +icons +dirvish)

       :term
       eshell
       (:if (not (memq system-type '(cygwin windows-nt ms-dos))) vterm)

       :checkers
       (syntax +childframe +icons +flymake)

       :tools
       direnv
       ;;editorconfig ;; Now built into Emacs.
       (eval +overlay)
       (lookup +dictionary +docsets +offline)
       (magit +forge)
       (lsp +peek +eglot)
       tree-sitter
       (docker +lsp)
       pdf
       biblio

       :lang
       (emacs-lisp +tree-sitter)
       (org +roam2 +tree-sitter +dragndrop +journal +pretty)
       (cc +lsp +tree-sitter)
       (web +lsp +tree-sitter)
       (sh +lsp +tree-sitter +powershell +fish)
       (nix +lsp +tree-sitter)
       (lua +lsp +tree-sitter +fennel)
       (data +lsp +tree-sitter)
       (latex +lsp +tree-sitter +cdlatex +latexmk +fold)
       (json +lsp +tree-sitter)
       (python +lsp +tree-sitter +pyright +cython)
       (haskell +lsp +tree-sitter)
       (graphql +lsp +tree-sitter)

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
       eat
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

       :secret
       ;;(:if (eq system-type 'gnu/linux) root)
       )

;;Local Variables:
;;eval: (when (featurep 'lispy) (lispy-mode -1))
;;End:
