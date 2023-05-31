;;; init.el -*- lexical-binding: t; -*-

(setq load-prefer-newer t)

(load! "lisp/cae-lib")
(load! "lisp/cae-hacks")

(setq doom-leader-alt-key "C-c"
      doom-localleader-alt-key "C-c y"
      doom-leader-key "SPC"
      doom-localleader-key "SPC l")
(setq native-comp-async-jobs-number (num-processors))

(doom! :completion
       (vertico +icons)

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

       :editor
       file-templates
       format
       snippets
       multiple-cursors

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
       emacs-lisp
       (org +roam2)
       (cc +lsp +tree-sitter)
       (web +lsp +tree-sitter)
       (sh +lsp +tree-sitter)
       (nix +lsp +tree-sitter)

       :os
       (:if IS-MAC macos)
       (:if (not (memq system-type '(cygwin windows-nt ms-dos))) tty +osc)

       :config
       (default +bindings +smartparens)

       :private
       ;; exwm
       (corfu +indexed)
       debugger
       (dirvish +icons +dirvish)
       eshell
       dired
       lisp
       misc-applications
       modeline
       (meow +leader)
       unpackaged
       vc)

;; Local Variables:
;; eval: (when (featurep 'lispy) (lispy-mode -1))
;; End:
