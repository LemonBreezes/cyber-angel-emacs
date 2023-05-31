;;; private/lisp/config.el -*- lexical-binding: t; -*-

(use-package! nameless
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'nameless-mode)
  :config
  (setq nameless-private-prefix t
        nameless-global-aliases '()))

(use-package! outline-minor-faces
  :hook (emacs-lisp-mode . outline-minor-faces-add-font-lock-keywords))

;; Check parens before saving.
(add-hook 'emacs-lisp-mode-hook #'cae-lisp-check-parens-before-save-h)
(add-hook 'lisp-mode-hook #'cae-lisp-check-parens-before-save-h)
(add-hook 'scheme-mode-hook #'cae-lisp-check-parens-before-save-h)
(add-hook 'clojure-mode-hook #'cae-lisp-check-parens-before-save-h)
(add-hook 'racket-mode-hook #'cae-lisp-check-parens-before-save-h)
(add-hook 'lfe-mode-hook #'cae-lisp-check-parens-before-save-h)
(add-hook 'hy-mode-hook #'cae-lisp-check-parens-before-save-h)
(add-hook 'dune-mode-hook #'cae-lisp-check-parens-before-save-h)
(add-hook 'fennel-mode-hook #'cae-lisp-check-parens-before-save-h)

;; This tool helps us a lot with regular expressions
(after! pcre2el
  (map! :prefix "C-c"
        (:prefix ("/" . "pcre2el")
         (:prefix ("e" . "elisp"))
         (:prefix ("p" . "pcre"))))
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
    (setq lispy-font-lock-keywords nil
          lispy-eval-display-style 'overlay
          lispy-no-permanent-semantic t) ;Semantic is slow and I don't know of
                                         ;any benefit it provides.
    (add-to-list 'lispy-elisp-modes #'minibuffer-mode)
    (add-to-list 'lispy-elisp-modes #'helpful-mode)
    (add-to-list 'lispy-elisp-modes #'lisp-data-mode)
    (add-hook 'helpful-mode #'lispy-mode)
    (map! :map lispy-mode-map-lispy
          ;; I prefer to keep the default `C-a' and `M-m' commands for moving to
          ;; the beginning of the line or indentation.
          "M-m" nil                     ;formerly `lispy-mark-symbol'. I prefer
          "C-a" nil                     ;formerly
                                        ;`lispy-move-beginning-of-line'.

          "C-e" nil                     ;formerly `lispy-move-end-of-line'.
                                        ;Doom's default `C-e' is better because
                                        ;it moves to the end of the code first
                                        ;and then to the end of the line rather
                                        ;than moving to the end of the line
                                        ;first.

          "M-i" nil                     ;formerly `lispy-iedit'. I prefer
                                        ;multiple cursors.

          ;; I prefer Doom's jump commands.
          "M-."   nil                   ;formerly `lispy-goto-symbol'.
          "C-M-," nil                   ;formerly `lispy-mark'.
          "M-,"   nil                   ;formerly `pop-tag-mark'.

          "M-r" #'lispy-raise-sexp
          "M-R" #'lispy-raise-some
          "M-S" #'lispy-split
          "M-C" #'lispy-convolute
          "M-D" #'lispy-splice
          "C-<backspace>" #'lispy-backward-kill-word ;This command normally
                                        ;to `M-<backspace>' but I
                                        ;have a Smartparens command
                                        ;bound to that key.
          "C-d" #'cae-delete-char
          ")" #'cae-insert-closing-paren)
    (when (modulep! :editor multiple-cursors)
      (after! multiple-cursors-core
        (add-to-list 'mc/unsupported-minor-modes 'lispy-mode)))))

;; The macrostep keymap is completely broken for me without this line. This
;; might be an Emacs30 thing.
(defvaralias 'macrostep-mode-map 'macrostep-mode-keymap)
