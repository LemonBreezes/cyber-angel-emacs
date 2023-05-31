;;; init.el -*- lexical-binding: t; -*-

(setq load-prefer-newer t)

(load! "lisp/cae-lib")
(load! "lisp/cae-hacks")

(setq doom-leader-alt-key "C-c"
      doom-localleader-alt-key "C-c l"
      doom-leader-key "SPC"
      doom-localleader-key "SPC l")
(setq native-comp-async-jobs-number (num-processors))

;; Also show keybinidng descriptions on my second leader key.
;; (defun +which-key-add-key-based-replacements-a
;;     (oldfun key-sequence &rest args)
;;   (when (string-prefix-p "SPC " key-sequence)
;;     (apply oldfun (concat "DEL " (string-remove-prefix "SPC " key-sequence))
;;            args))
;;   (apply oldfun key-sequence args))
;; (advice-add #'which-key-add-key-based-replacements :around
;;             #'+which-key-add-key-based-replacements-a)

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
       vc

       :term
       eshell
       (:if (not (memq system-type '(cygwin windows-nt ms-dos))) vterm)

       :checkers
       (syntax +childframe)
       spell
       grammar

       :tools
       direnv
       debugger
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

       :os
       (:if IS-MAC macos)
       (:if (not (memq system-type '(cygwin windows-nt ms-dos))) tty +osc)

       :config
       (default +bindings +smartparens)

       :private
       ;; exwm
       (corfu +indexed)
       ;; debugger
       (dirvish +icons +dirvish)
       eshell
       dired
       lisp
       misc-applications
       helm
       modeline
       meow
       unpackaged
       vc)

;; Local Variables:
;; eval: (when (featurep 'lispy) (lispy-mode -1))
;; End:
