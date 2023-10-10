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
  (defalias 'sp-up-sexp #'up-list)
  (defalias 'sp-backward-up-sexp #'backward-up-list)
  (defalias 'sp-raise-sexp #'raise-sexp)
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
    (add-to-list 'sp-ignore-modes-list #'inferior-emacs-lisp-mode)

    ;; I prefer for `C-M-n' and `C-M-p' to never act like `sp-backward-up-sexp' or
    ;; `sp-up-sexp'.
    (setq sp-navigate-interactive-always-progress-point t)

    (setq sp-navigate-reindent-after-up '()
          sp-navigate-reindent-after-up-in-string nil)

    (when (and (modulep! :editor evil)
               (not (modulep! :editor lispy)))
      (use-package! evil-cleverparens
        :defer t :init
        (setq evil-cp-additional-movement-keys
              '(("M-l" . evil-cp-end-of-defun)
                ("M-h" . evil-cp-beginning-of-defun)))
        (dolist (mode sp-lisp-modes)
          (add-hook mode (cae-defun cae-enable-evil-cleverparens-mode ()
                           (evil-cleverparens-mode +1))))
        (add-hook 'prog-mode-hook #'cae-enable-evil-cleverparens-mode)

        :custom ((evil-cleverparens-use-s-and-S nil)
                 (evil-cleverparens-swap-move-by-word-and-symbol nil))
        :config
        (map! :map evil-cleverparens-mode-map
              :n "M-\"" #'cae-evil-cp-wrap-next-double-quotes
              [remap sp-raise-sexp] #'cae-sp-raise-sexp)))

    (map! :map narrow-map
          "s" #'sp-narrow-to-sexp)

    (dolist (binding '(("C-M-t" . sp-transpose-sexp)
                       ("C-M-k" . sp-kill-sexp)
                       ("C-M-S-k" . sp-kill-hybrid-sexp)
                       ("C-S-w" . sp-backward-kill-sexp)
                       ("C-M-f" . sp-forward-sexp)
                       ("C-M-b" . sp-backward-sexp)
                       ("C-M-u" . sp-backward-up-sexp)
                       ("C-M-d" . sp-down-sexp)
                       ("C-M-e" . sp-up-sexp)
                       ("C-M-a" . sp-backward-down-sexp)
                       ("C-M-n" . sp-next-sexp)
                       ("C-M-p" . sp-previous-sexp)
                       ("C-M-S-r" . cae-sp-raise-sexp)
                       ("C-M-S-s" . sp-split-sexp)
                       ("C-M-S-c" . sp-convolute-sexp)
                       ("C-M-S-d" . sp-splice-sexp)
                       ("C-)" . sp-forward-slurp-sexp)
                       ("C-M-)" . sp-slurp-hybrid-sexp)
                       ("C-(" . sp-backward-slurp-sexp)
                       ("C-}" . sp-forward-barf-sexp)
                       ("C-{" . sp-backward-barf-sexp)
                       ("C-x C-t" . sp-transpose-hybrid-sexp)
                       ("M-<delete>" . sp-unwrap-sexp)
                       ("M-<backspace>" . sp-backward-unwrap-sexp)
                       ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
                       ("C-M-<delete>" . sp-splice-sexp-killing-forward)
                       ;; These are redundant with previous keybindings.
                       ;;("C-M-S-<backspace>" . sp-backward-kill-sexp)
                       ;;("C-M-S-<delete>" . sp-kill-sexp)

                       ;;("C-]" . sp-select-next-thing-exchange) ;Overrides `abort-recursive-edit'.
                       ;;("C-M-]" . sp-select-next-thing)
                       ;;("C-M-@" . sp-mark-sexp)
                       ("C-M-S-w" . sp-copy-sexp)))
      (define-key smartparens-mode-map (kbd (car binding)) (cdr binding))
      (when (modulep! :editor lispy)
        (after! lispy
          (unless (lookup-key lispy-mode-map (kbd (car binding)))
            (define-key lispy-mode-map (kbd (car binding)) (cdr binding)))))))

  (map! :map smartparens-mode-map "C-M-?" #'cae-sp-which-key-cheatsheet))
