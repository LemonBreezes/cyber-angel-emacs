;;; private/lisp/config.el -*- lexical-binding: t; -*-

(use-package! nameless
  :defer t
  ;; `nameless-mode' can cause buffers to become garbled when used in a
  ;; terminal.
  :when (cae-display-graphic-p)
  :init
  (add-hook 'emacs-lisp-mode-hook #'nameless-mode)
  (after! elisp-mode
    (map! :map emacs-lisp-mode-map
          :localleader
          "n" #'nameless-mode))
  :config
  (setq nameless-private-prefix t
        nameless-global-aliases '()))

(add-hook! 'emacs-lisp-mode-hook
  (setq-local elisp-flymake-byte-compile-load-path
              load-path))

;; Show `eros' overlays for `edebug' results.
(when (modulep! :tools eval +overlay)
  (defun cae-edebug-compute-previous-result-a (_ &rest r)
    "Adviced `edebug-compute-previous-result'."
    (let ((previous-value (nth 0 r)))
      (if edebug-unwrap-results
          (setq previous-value
                (edebug-unwrap* previous-value)))
      (setq edebug-previous-result
            (edebug-safe-prin1-to-string previous-value))))

  (defun cae-edebug-previous-result-a (_ &rest r)
    "Adviced `edebug-previous-result'."
    (eros--make-result-overlay edebug-previous-result
      :where (point)
      :duration eros-eval-result-duration))
  (advice-add #'edebug-compute-previous-result
              :around
              #'adviced:edebug-compute-previous-result)
  (advice-add #'edebug-previous-result
              :around
              #'adviced:edebug-previous-result))

;; Allow `eval-expression' to have comments.
(add-hook 'minibuffer-setup-hook
          (cae-defun cae-lisp-eval-expression-set-up-comments-h ()
            (when (string= (minibuffer-prompt) "Eval: ")
              (setq-local comment-start ";"
                          comment-end ""
                          comment-start-skip ";+ *"
                          comment-end-skip "[ 	]*\\(\\s>\\|\n\\)"))))

;; Allow inserting newlines in the minibuffer. Also protect from
;; entering unbalanced expressions into `eval-expression'.
(map! :map minibuffer-local-map
      [remap exit-minibuffer] #'cae-lisp-newline-and-indent)
(after! lispy
  (setq lispy-avy-keys avy-keys)
  (map! :map lispy-mode-map
        [remap lispy-newline-and-indent-plain] #'cae-lisp-newline-and-indent))

;; Use Emacs Lisp mode for dir-locals files.
(add-to-list 'auto-mode-alist (cons (regexp-quote dir-locals-file)
                                    'emacs-lisp-mode))

;; Use my own Imenu expression instead of Doom's. For mine, I do not count
;; comments like ";; This code does ..." as sections.
(advice-add #'+emacs-lisp-extend-imenu-h :override #'cae-emacs-lisp-extend-imenu-h)

;; Check parens before saving.
(dolist (mode sp-lisp-modes)
  (add-hook mode #'cae-lisp-check-parens-before-save-h))

;; This tool helps us a lot with regular expressions
(after! pcre2el
  (after! which-key
    (which-key-add-keymap-based-replacements rxt-mode-map
      "C-c /" "pcre2el"
      "C-c / e" "elisp"
      "C-c / p" "pcre"))
  (map! :map rxt--read-pcre-mode-map
        "C-c C-i" #'rxt--toggle-i-mode
        "C-c C-t" #'rxt--toggle-s-mode
        "C-c C-x" #'rxt--toggle-x-mode)
  (undefine-key! rxt--read-pcre-mode-map
    "C-c i" "C-c s" "C-c x"))
(add-hook 'emacs-lisp-mode-hook #'rxt-mode)

;;(use-package! page-break-lines
;;  :defer t :init (add-hook 'emacs-lisp-mode-hook #'page-break-lines-mode))

;; This fixes aggressive indent's indentation of plists. :)
(after! lisp-mode
  (defalias 'lisp-indent-function '+emacs-lisp-indent-function))

(when (modulep! :editor lispy)
  (after! lispy
    (add-hook 'doom-escape-hook
              (cae-defun cae-lispy-clear-iedit-h ()
                (when (bound-and-true-p iedit-mode)
                  (iedit-mode -1) t)))
    (setq lispy-font-lock-keywords nil
          lispy-eval-display-style 'overlay
          lispy-no-permanent-semantic t ;Semantic is slow and I don't know of
                                        ;any benefit it provides.
          lispy-avy-style-char 'at
          lispy-avy-style-symbol 'at)
    (add-to-list 'lispy-elisp-modes #'minibuffer-mode)
    (add-to-list 'lispy-elisp-modes #'helpful-mode)
    (add-to-list 'lispy-elisp-modes #'lisp-data-mode)
    (add-hook 'helpful-mode #'lispy-mode)
    (map! :map lispy-mode-map-lispy
          (:when (not (modulep! :editor evil))
           ;; I prefer to keep the default `C-a' and `M-m' commands for moving to
           ;; the beginning of the line or indentation.
           "M-m" nil                    ;formerly `lispy-mark-symbol'.
           "C-a" nil                    ;formerly
                                        ;`lispy-move-beginning-of-line'.

           "C-e" nil                    ;formerly `lispy-move-end-of-line'.
                                        ;Doom's default `C-e' is better because
                                        ;it moves to the end of the code first
                                        ;and then to the end of the line rather
                                        ;than moving to the end of the line
                                        ;first.

           "M-i" nil                    ;formerly `lispy-iedit'. I prefer
                                        ;multiple cursors.

           ;; I prefer Doom's jump commands.
           "M-." nil                    ;formerly `lispy-goto-symbol'.
           "C-M-," nil                  ;formerly `lispy-mark'.
           "M-," nil                    ;formerly `pop-tag-mark'.

           "M-R" #'lispy-raise-sexp
           "M-S" #'lispy-split
           "M-C" #'lispy-convolute
           "M-D" #'lispy-splice)
          (:when (modulep! :editor evil)
           "M-," nil)
          "C-d" #'cae-delete-char
          ")" #'cae-insert-closing-paren))

  (when (modulep! :editor evil)
    (setq lispyville-key-theme
          '(commentary
            (operators normal)
            c-w
            c-u
            (atom-movement t)
            slurp/barf-lispy
            additional
            additional-insert))
    (after! lispyville
      (cl-pushnew '(lispyville-comment-or-uncomment :face evil-goggles-commentary-face :switch
                    evil-goggles-enable-commentary :advice
                    evil-goggles--generic-async-advice)
                  evil-goggles--commands)
      (setq lispyville-motions-put-into-special t))))
