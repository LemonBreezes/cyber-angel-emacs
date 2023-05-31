;;; init.el -*- lexical-binding: t; -*-

(load! "lisp/cae-hacks")

(setq doom-leader-alt-key "C-c"
      doom-localleader-alt-key "C-c l")

(doom! :completion
       (vertico +icons)

       :ui
       doom-dashboard
       (:if (not (memq system-type '(cygwin windows-nt ms-dos)))
           (emoji +unicode +github +ascii))
       hl-todo
       hydra
       (ligatures +extra +iosevka)
       nav-flash
       (popup +defaults)
       ophints
       (vc-gutter +pretty +diff-hl)
       vi-tilde-fringe
       workspaces

       :editor
       multiple-cursors
       file-templates
       snippets

       :emacs
       undo
       (ibuffer +icons)

       :term
       eshell
       (:if (not (memq system-type '(cygwin windows-nt ms-dos))) vterm)

       :checkers
       (syntax +childframe)
       spell
       grammar

       :tools
       (debugger +lsp)
       editorconfig
       (eval +overlay)
       (lookup +dictionary +docsets +offline)
       magit
       (lsp +peek)
       tree-sitter

       :lang
       emacs-lisp
       (org +roam2)
       (cc +lsp +tree-sitter)

       :os
       (:if IS-MAC macos)
       (:if (not (memq system-type '(cygwin windows-nt ms-dos))) tty +osc)

       :config
        (default +bindings)

       :private
       ;; exwm
       (corfu +tng)
       debugger
       (dirvish +icons +dirvish)
       eshell
       dired
       lisp
       misc-applications
       helm
       modeline
       unpackaged
       vc)

;; Local Variables:
;; eval: (when (featurep 'lispy) (lispy-mode -1))
;; End:
