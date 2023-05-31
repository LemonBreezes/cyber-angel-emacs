;;; init.el -*- lexical-binding: t; -*-

(setq doom-leader-alt-key "C-c"
      doom-localleader-alt-key "C-c l")

(doom! :completion
       (vertico +icons +childframe)

       :ui
       doom-dashboard
       (:if (not (memq system-type '(cygwin windows-nt ms-dos)))
           (emoji +unicode +github +ascii))
       hl-todo
       hydra
       (ligatures +extra +iosevka)
       (modeline +light)
       (popup +defaults)
       ophints
       (vc-gutter +pretty)
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
       debugger
       editorconfig
       (eval +overlay)
       lookup
       magit
       (lsp +eglot)
       tree-sitter

       :lang
       emacs-lisp
       org

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
