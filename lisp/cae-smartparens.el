;;; lisp/cae-smartparens.el -*- lexical-binding: t; -*-

;; Make `smartparens' optional.
(unless (modulep! :config default +smartparens)
  ;; Check this out. This guy removes
  ;; https://git.sr.ht/~alternateved/dotemacs/tree/main/item/init.el#L404
  ;; I think you could achieve similar effects with puni (which does a bit too
  ;; much for my taste) or paredit (which supposedly works with non-lisps to
  ;; some regard), but I prefer those small utility functions
  (provide 'smartparens)
  (defalias 'sp-local-pair #'ignore)
  (defalias 'sp-pair #'ignore)
  (defalias 'sp-with-modes #'ignore)
  (defalias 'sp--syntax-ppss #'syntax-ppss)
  (defalias 'sp-point-in-comment
    (lambda (&optional pos) (nth 4 (syntax-ppss pos))))
  (defalias 'sp-point-in-string
    (lambda (&optional pos) (nth 3 (syntax-ppss pos))))
  (defalias 'sp-beginning-of-sexp
    (lambda (&optional arg) (goto-char (beginning-of-thing 'sexp))))
  (remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
  (when (modulep! :editor lispy)
    (after! lispy
      (remove-hook 'lispy-mode-hook #'turn-off-smartparens-mode)))
  (when (modulep! :term eshell)
    (after! eshell
      (remove-hook 'eshell-mode-hook #'smartparens-mode)))
  (when (modulep! :editor snippets)
    (remove-hook 'yas-before-expand-snippet-hook
                 #'+snippets--disable-smartparens-before-expand-h))

  ;; This is how we get curly braces working in C without `smartparens'.
  (setq electric-pair-pairs '((?\" . ?\")
                              (?\{ . ?\})
                              (?\[ . ?\]))
        electric-pair-inhibit-predicate #'electric-pair-conservative-inhibit
        electric-pair-open-newline-between-pairs t)
  (electric-pair-mode +1)
  (map! [remap newline] nil))

(when (modulep! :config default +smartparens)
  (after! smartparens
    (sp-local-pair 'org-mode "<<" ">>")

    ;; I prefer for `C-M-n' and `C-M-p' to never act like `sp-backward-up-sexp' or
    ;; `sp-up-sexp'.
    (setq sp-navigate-interactive-always-progress-point t)

    (when (and (modulep! :editor evil)
               (not (modulep! :editor lispy)))
      (use-package! evil-cleverparens
        :defer t :init
        (setq evil-cp-additional-movement-keys
              '(("M-l" . evil-cp-end-of-defun)
                ("M-h" . evil-cp-beginning-of-defun)))
        (add-hook 'prog-mode-hook 'evil-cleverparens-mode)
        :custom ((evil-cleverparens-use-s-and-S nil)
                 (evil-cleverparens-swap-move-by-word-and-symbol t))
        :config
        (map! :map evil-cleverparens-mode-map
              :n "M-\"" #'+evil-cp-wrap-next-double-quotes
              :n "<f6>" #'cae-evil-cleverparens-hydra/body)))

    (let ((bindings
           `(;; These are safe to have globally
             ("C-M-t" sp-transpose-sexp "Edit")
             ("C-x C-t" sp-transpose-hybrid-sexp "Edit")
             ("C-M-k" sp-kill-sexp "Delete")
             ("C-M-S-k" sp-kill-hybrid-sexp "Delete")
             ("C-M-f" sp-forward-sexp "Move")
             ("C-M-b" sp-backward-sexp "Move")
             ("C-M-u" sp-backward-up-sexp "Move")
             ("C-M-d" sp-down-sexp "Move")
             ("C-M-a" sp-beginning-of-sexp "Move")
             ("C-M-e" sp-up-sexp "Move")
             ("C-M-a" sp-backward-down-sexp "Move")
             ("C-M-n" sp-next-sexp "Move")
             ("C-M-p" sp-previous-sexp "Move")
             ("C-M-R" cae-raise-sexp "Delete")
             ("C-)" sp-forward-slurp-sexp "Barf/Slurp")
             ("C-M-)" sp-slurp-hybrid-sexp "Barf/Slurp")
             ("C-(" sp-backward-slurp-sexp "Barf/Slurp")
             ("C-}" sp-forward-barf-sexp "Barf/Slurp")
             ("C-{" sp-backward-barf-sexp "Barf/Slurp")
             ,@(unless (modulep! :editor evil)
                 '(("M-(" sp-wrap-round "Edit")
                   ("M-S" sp-split-sexp "Edit")
                   ("M-J" sp-join-sexp "Edit")
                   ("M-D" sp-splice-sexp "Delete")
                   ("M-C" sp-convolute-sexp "Edit")))
             ;; These below override built-in commands and I think with
             ;; `evil-cleverparens', they are not needed.
             ("M-<delete>" sp-unwrap-sexp "Delete")
             ("M-<backspace>" sp-backward-unwrap-sexp "Delete")
             ("C-M-<backspace>" sp-splice-sexp-killing-backward "Delete")
             ("C-M-<delete>" sp-splice-sexp-killing-forward "Delete")
             ("C-M-S-<backspace>" sp-backward-kill-sexp "Delete")
             ("C-M-S-<delete>" sp-kill-sexp "Delete")
             ;;("C-]" sp-select-next-thing-exchange "Select") ;Overrides
                                        ;`abort-recursive-edit'.
             ;;("C-M-]" sp-select-next-thing "Select")
             ;;("C-M-@" sp-mark-sexp "Select")
             ;;("C-M-S-w" sp-copy-sexp "Select")
             )))
      (when (modulep! :ui hydra)
        (eval
         `(defun cae-sp-cheat-sheet/body ()
            (interactive)
            ,(append
              `(defhydra cae-sp-cheat-sheet (:hint nil :foreign-keys run)
                 ;; If we're using Lispy, we want `<f6>' to be a cheatsheet for
                 ;; that instead.
                 (,(if (modulep! :editor lispy) "C-M-?" "<f6>") nil "Exit" :exit t))
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
        (define-key smartparens-mode-map (kbd "<f6>") #'cae-sp-cheat-sheet/body))
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
      (global-set-key (kbd (if (modulep! :editor lispy) "C-M-?" "<f6>")) #'cae-sp-cheat-sheet/body))))
