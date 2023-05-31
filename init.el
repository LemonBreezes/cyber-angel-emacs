;;; init.el -*- lexical-binding: t; -*-

(setq doom-leader-alt-key "C-c"
      doom-localleader-alt-key "C-c l")

(doom! :completion
       (vertico +icons)

       :ui
       (:if (not (memq system-type '(cygwin windows-nt ms-dos))) (emoji +unicode +github +ascii))
       (ligatures +extra +iosevka)
       vi-tilde-fringe

       :emacs
       undo

       :tools
       magit
       tree-sitter

       :lang
       emacs-lisp

       :os
       (:if IS-MAC macos)
       (:if (not (memq system-type '(cygwin windows-nt ms-dos))) tty +osc)

       :private
       (dirvish +icons +dirvish)
       dired)

;; Local Variables:
;; eval: (when (featurep 'lispy) (lispy-mode -1))
;; End:
