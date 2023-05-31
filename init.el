;;; init.el -*- lexical-binding: t; -*-

;; For when we compile Doom.
(defvar personal-keybindings nil)

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
       modeline
       (popup +defaults)
       ophints
       (vc-gutter +pretty +diff-hl)
       vi-tilde-fringe
       workspaces

       :editor
       multiple-cursors
       file-templates
       snippets
       word-wrap

       :emacs
       undo

       :term
       eshell

       :checkers
       (syntax +childframe)
       (spell +everywhere)

       :tools
       (debugger +lsp)
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

       :os
       (:if IS-MAC macos)
       (:if (not (memq system-type '(cygwin windows-nt ms-dos))) tty +osc)

       :config
        (default +bindings)

       :private
       ;; exwm
       (corfu +tng)
       (dirvish +icons +dirvish)
       dired
       lisp
       unpackaged
       vc)

;; Local Variables:
;; eval: (when (featurep 'lispy) (lispy-mode -1))
;; End:
