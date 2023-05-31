;;; init.el -*- lexical-binding: t; -*-

(setq load-prefer-newer t)

(load! "lisp/cae-hacks")

(setq doom-leader-alt-key "C-c"
      doom-localleader-alt-key "C-c l")
(setq native-comp-async-jobs-number (num-processors))

(doom! :completion
       (vertico +icons +childframe)

       :ui
       doom-dashboard
       hl-todo
       hydra
       (ligatures +extra +iosevka)
       nav-flash
       (popup +defaults)
       (vc-gutter +pretty +diff-hl)
       vi-tilde-fringe
       workspaces

       :editor
       multiple-cursors
       file-templates
       format
       snippets

       :emacs
       undo
       ibuffer

       :term
       eshell
       (:if (not (memq system-type '(cygwin windows-nt ms-dos))) vterm)

       :checkers
       (syntax +childframe +flymake)
       spell
       grammar

       :tools
       direnv
       debugger
       editorconfig
       (eval +overlay)
       (lookup +dictionary +docsets +offline)
       magit
       (lsp +peek +eglot)
       tree-sitter

       :lang
       emacs-lisp
       (org +roam2)
       (cc +lsp +tree-sitter)
       (web +lsp +tree-sitter)
       (sh +lsp +tree-sitter)

       :os
       (:if IS-MAC macos)
       (:if (not (memq system-type '(cygwin windows-nt ms-dos))) tty +osc)

       :config
       (default +bindings)

       :private
       ;; exwm
       (corfu +tng)
       ;; debugger
       (dirvish +icons +dirvish)
       ;; eshell
       dired
       lisp
       misc-applications
       ;; helm
       modeline
       unpackaged
       vc)

;; Local Variables:
;; eval: (when (featurep 'lispy) (lispy-mode -1))
;; End:
