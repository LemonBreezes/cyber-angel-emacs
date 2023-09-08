;;; lisp/smartparens-hydra.el -*- lexical-binding: t; -*-

(let ((bindings
       '(("M-(" sp-wrap-round "Edit")
         ("M-S" sp-split-sexp "Edit")
         ("M-J" sp-join-sexp "Edit")
         ("M-C" sp-convolute-sexp "Edit")
         ("C-M-t" sp-transpose-sexp "Edit")
         ("C-x C-t" sp-transpose-hybrid-sexp "Edit")
         ("C-M-k" sp-kill-sexp "Delete")
         ("C-M-S-k" sp-kill-hybrid-sexp "Delete")
         ("M-<delete>" sp-unwrap-sexp "Delete")
         ("M-<backspace>" sp-backward-unwrap-sexp "Delete")
         ("C-M-R" cae-raise-sexp "Delete")
         ("M-D" sp-splice-sexp "Delete")
         ("C-M-<backspace>" sp-splice-sexp-killing-backward "Delete")
         ("C-M-<delete>" sp-splice-sexp-killing-forward "Delete")
         ("C-M-S-<backspace>" sp-backward-kill-sexp "Delete")
         ("C-M-f" sp-forward-sexp "Move")
         ("C-M-b" sp-backward-sexp "Move")
         ("C-M-u" sp-backward-up-sexp "Move")
         ("C-M-d" sp-down-sexp "Move")
         ("C-M-a" sp-beginning-of-sexp "Move")
         ("C-M-e" sp-up-sexp "Move")
         ("C-M-a" sp-backward-down-sexp "Move")
         ("C-M-n" sp-next-sexp "Move")
         ("C-M-p" sp-previous-sexp "Move")
         ("C-(" sp-forward-slurp-sexp "Barf/Slurp")
         ("C-<right>" sp-forward-slurp-sexp "Barf/Slurp")
         ("C-S-<right>" sp-slurp-hybrid-sexp "Barf/Slurp")
         ("C-}" sp-forward-barf-sexp "Barf/Slurp")
         ("C-<left>" sp-forward-barf-sexp "Barf/Slurp")
         ("C-{" sp-backward-barf-sexp "Barf/Slurp")
         ("C-M-<right>" sp-backward-slurp-sexp "Barf/Slurp")
         ;;("C-]" sp-select-next-thing-exchange "Select") ;Overrides
                                    ;`abort-recursive-edit'.
         ;;("C-M-]" sp-select-next-thing "Select")
         ;;("C-M-@" sp-mark-sexp "Select")
         ("C-M-S-w" sp-copy-sexp "Select"))))
  (when (modulep! :ui hydra)
    (eval
     `(defun cae-sp-cheat-sheet/body ()
        (interactive)
        ,(append
          '(defhydra cae-sp-cheat-sheet (:hint nil :foreign-keys run)
             ("C-M-?" nil "Exit" :exit t))
          (cl-loop for x in bindings
                   collect (list (car x)
                                 (cadr x)
                                 (thread-last (symbol-name (cadr x))
                                              (string-remove-prefix "cae-")
                                              (string-remove-prefix "sp-"))
                                 :column
                                 (caddr x))))
        (cae-sp-cheat-sheet/body))
     t)
    (define-key smartparens-mode-map (kbd "C-M-?") #'cae-sp-cheat-sheet/body))
  (when (modulep! :editor multiple-cursors)
    (after! multiple-cursors-core
      (dolist (it sp--mc/cursor-specific-vars)
        (add-to-list 'mc/cursor-specific-vars it))
      (dolist (x bindings)
        (add-to-list 'mc/cmds-to-run-for-all (cadr x))
        (add-to-list 'mc/cmds-to-run-for-all
                     (intern (concat "cae-sp-cheat-sheet/"
                                     (symbol-name (cadr x))))))
      (add-to-list 'mc/cmds-to-run-once #'cae-sp-cheat-sheet/body)
      (add-to-list 'mc/cmds-to-run-once #'cae-sp-cheat-sheet/nil)))
  (dolist (x bindings)
    (define-key smartparens-mode-map (kbd (car x)) (cadr x))
    ;; Prevent our commands from being shadowed by `smartparens-mode-map'.
    ;;(define-key smartparens-mode-map (kbd (car x)) nil)
    ;;(global-set-key (kbd (car x)) (cadr x))
    )
  ;; I define this key globally so that I can always reference the
  ;; `smartparens' keymap and use it as a hydra even if the mode is not
  ;; active.
  (global-set-key (kbd "C-M-?") #'cae-sp-cheat-sheet/body))
