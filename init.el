;;; init.el -*- lexical-binding: t; -*-

;; Don't enable these options. They will break your config.
(defvar cae-config-compilation-on-kill-enabled-p nil
  "Whether on-kill native compilation is enabled.")
(defvar cae-config-incremental-compilation-enabled-p nil
  "Whether incremental native compilation is enabled.")

;;Since I sometimes compile my config, I want to make sure that I'm always
;;loading the latest version.
(setq load-prefer-newer (not (or cae-config-compilation-on-kill-enabled-p
                                 cae-config-incremental-compilation-enabled-p)))
(random t)                              ;Set the random seed.
(setq doom-theme 'wheatgrass)           ;Set a dark fallback theme.

(load! "lisp/cae-debug")                ;Debug Emacs.
(load! "lisp/cae-lib")
(load! "lisp/cae-hacks")

(setq doom-leader-alt-key "M-SPC"
      doom-localleader-alt-key "M-SPC m"
      doom-leader-key "C-c"
      doom-localleader-key "C-c l")
(setq native-comp-async-jobs-number (num-processors))

(defvar cae-init-ui-enabled-p t
  "Whether our UI section of `config.el' is disabled.")
(defvar cae-init-tools-enabled-p t
  "Whether our tools section of `config.el' is disabled.")
(defvar cae-init-editor-enabled-p t
  "Whether our editor section of `config.el' is disabled.")
(defvar cae-init-autocompletion-enabled-p t
  "Whether our autocompletion section of `config.el' is disabled.")

(condition-case err
    (doom! :completion
           (vertico +icons +childframe)

           :ui
           doom-dashboard
           hl-todo
           hydra
           ;; (ligatures +extra +iosevka)
           nav-flash
           (popup +defaults)
           (vc-gutter +pretty +diff-hl)
           vi-tilde-fringe
           workspaces
           ;; (window-select +switch-window)

           :editor
           file-templates
           format
           snippets
           multiple-cursors
           lispy

           :emacs
           undo
           ibuffer
           vc

           :term
           eshell
           (:if (not (memq system-type '(cygwin windows-nt ms-dos))) vterm)

           :checkers
           (syntax +childframe)
           spell
           grammar

           :tools
           (debugger +lsp)
           direnv
           editorconfig
           (eval +overlay)
           (lookup +dictionary +docsets +offline)
           (magit +forge)
           (lsp +peek)
           tree-sitter

           :lang
           (emacs-lisp +tree-sitter)
           (org +roam2 +tree-sitter)
           (cc +lsp +tree-sitter)
           (web +lsp +tree-sitter)
           (sh +lsp +tree-sitter)
           (nix +lsp +tree-sitter)

           :os
           (:if IS-MAC macos)
           (:if (not (memq system-type '(cygwin windows-nt ms-dos))) tty +osc)

           :config
           (default +bindings +smartparens)

           :email
           ;; (mu4e +org)

           :app
           (rss +org)

           :private
           (corfu +indexed)
           debugger
           (dirvish +icons +dirvish)
           eshell
           dired
           lisp
           misc-applications
           modeline
           unpackaged
           vc
           org

           ;; (meow +leader)
           )
  (progn
    ;; This comment will be removed after I test this.
    (message "Error: %S" err)
    (with-output-to-temp-buffer "*Backtrace*"
      (backtrace))
    (ido-mode 1)))

;; Local Variables:
;; eval: (when (featurep 'lispy) (lispy-mode -1))
;; End:
