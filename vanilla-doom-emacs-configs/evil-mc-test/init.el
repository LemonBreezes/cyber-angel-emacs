;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       (vertico +icons)
       (corfu +orderless +icons +dabbrev)

       :editor
       multiple-cursors
       (evil +everywhere)

       :config
       (default +bindings +smartparens))

;;Local Variables:
;;eval: (when (featurep 'lispy) (lispy-mode -1))
;;End:
