;;; cae/lisp/config.el -*- lexical-binding: t; -*-

(require 'cae-lib)

;; Show `eros' overlays for `edebug' results.
(when (modulep! :tools eval +overlay)
  (advice-add #'edebug-compute-previous-result :around
              #'cae-edebug-compute-previous-result-a)
  (advice-add #'edebug-previous-result :around #'cae-edebug-previous-result-a))

;; Allow deleting a closing paren if parens are unbalanced. Also allow inserting
;; a closing paren if parens are unbalanced.
(map! [remap delete-char] #'cae-delete-char
      ")" #'cae-insert-closing-paren)

;; Allow C-u - using `pp' on the `eval-expression' output.
(defvaralias 'pp-read-expression-map 'minibuffer-local-map)
(map! [remap eval-last-sexp] #'cae-eval-last-sexp
      [remap eval-expression] #'cae-eval-expression)
(add-hook! 'eros-mode-hook
  (defun cae-eros-setup-keybindings-h ()
    (map! [remap eval-last-sexp] #'cae-eval-last-sexp)))

;; Allow inserting newlines in the minibuffer. Also protect from
;; entering unbalanced expressions into `eval-expression'.
(map! :map minibuffer-local-map
      [remap exit-minibuffer] #'cae-lisp-newline-and-indent)
(after! lispy
  (setq lispy-avy-keys avy-keys)
  (map! :map lispy-mode-map
        [remap lispy-newline-and-indent-plain] #'cae-lisp-newline-and-indent))

;; Use Emacs Lisp mode for dir-locals files. This will make them easier to edit.
(add-to-list 'auto-mode-alist (cons (regexp-quote dir-locals-file)
                                    'emacs-lisp-mode))

;; Use my own Imenu expression instead of Doom's. For mine, I do not count
;; comments like ";; This code does ..." as sections.
(advice-add #'+emacs-lisp-extend-imenu-h :override #'cae-emacs-lisp-extend-imenu-h)

;; Check parens before saving.
(after! smartparens
  (dolist (mode '(cider-repl-mode
                  clojure-mode
                  clojurec-mode
                  clojurescript-mode
                  clojurex-mode
                  clojure-ts-mode
                  clojurescript-ts-mode
                  clojurec-ts-mode
                  common-lisp-mode
                  emacs-lisp-mode
                  eshell-mode
                  fennel-mode
                  fennel-repl-mode
                  geiser-repl-mode
                  gerbil-mode
                  inf-clojure-mode
                  inferior-emacs-lisp-mode
                  inferior-lisp-mode
                  inferior-scheme-mode
                  lisp-interaction-mode
                  lisp-mode
                  monroe-mode
                  racket-mode
                  racket-repl-mode
                  scheme-interaction-mode
                  scheme-mode
                  slime-repl-mode
                  sly-mrepl-mode
                  stumpwm-mode
                  ))
    (add-hook (intern (concat (symbol-name mode) "-hook")) #'cae-lisp-check-parens-before-save-h)))

;; This fixes aggressive indent's indentation of plists. :)
;;(after! lisp-mode
;;  (defalias 'lisp-indent-function '+emacs-lisp-indent-function))

(when (modulep! :editor lispy)
  (after! lispy
    ;;(add-hook! 'doom-escape-hook
    ;;  (defun cae-lispy-clear-iedit-h ()
    ;;    (when (bound-and-true-p iedit-mode)
    ;;      (iedit-mode -1) t)))
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

           "M-i" nil)                   ;formerly `lispy-iedit'. I prefer
                                        ;multiple cursors.
          ;; I used to use these commands but now I have some `smartparens'
          ;; keybindings for them.
          ;;:ie "M-R" #'lispy-raise-sexp
          ;;:ie "M-S" #'lispy-split
          ;;:ie "M-C" #'lispy-convolute
          ;;:ie "M-D" #'lispy-splice
          ;; I prefer Doom's jump commands.
          "M-." nil                     ;formerly `lispy-goto-symbol'.
          "C-M-," nil                   ;formerly `lispy-mark'.
          "M-," nil                     ;formerly `pop-tag-mark'.
          (:when (modulep! :editor evil)
           "M-," nil)
          "C-d" #'cae-delete-char
          "C-M-?" #'cae-lispy-which-key-cheatsheet
          ")" #'cae-insert-closing-paren
          "<f7>" #'cae-lispy-cheatsheet)
    (map! :map help-map
          ;; You can use this since it dynamically updates, but the Lispy
          ;; cheatsheet Hydra is better.
          ;;"bl" #'cae-lispy-which-key-cheatsheet
          ))

  (when (modulep! :editor evil)
    (setq lispyville-key-theme
          '(commentary
            (operators normal)
            c-w
            c-u
            (prettify insert)
            (atom-movement t)
            slurp/barf-lispy
            additional
            additional-insert))
    (when (modulep! :ui ophints)
      (after! evil-goggles
        (cl-pushnew '(lispyville-comment-or-uncomment :face evil-goggles-commentary-face :switch
                      evil-goggles-enable-commentary :advice
                      evil-goggles--generic-async-advice)
                    evil-goggles--commands)
        (setq lispyville-motions-put-into-special t)))
    (after! lispyville
      (map! :map lispyville-mode-map
            :n "M-s" nil                     ;formerly `lispy-splice'.
            ))))
