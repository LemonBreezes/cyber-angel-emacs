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
    (setq lispy-font-lock-keywords nil)
    (setq! lispy-eval-display-style 'overlay)
    (add-to-list 'lispy-elisp-modes #'minibuffer-mode)
    (add-to-list 'lispy-elisp-modes #'helpful-mode)
    (add-to-list 'lispy-elisp-modes #'lisp-data-mode)
    (add-hook 'helpful-mode #'lispy-mode)
    (when (modulep! :ui hydra)
      (let ((bindings `(("<" "lispy-barf" "")
                        (,(cae-keyboard-kbd "A") "lispy-beginning-of-defun" "")
                        (,(cae-keyboard-kbd "j") "lispy-down" "")
                        (,(cae-keyboard-kbd "Z") "lispy-edebug-stop" "")
                        (,(cae-keyboard-kbd "B") "lispy-ediff-regions" "")
                        (,(cae-keyboard-kbd "G") "lispy-goto-local" "")
                        (,(cae-keyboard-kbd "h") "lispy-left" "")
                        (,(cae-keyboard-kbd "N") "lispy-narrow" "")
                        (,(cae-keyboard-kbd "y") "lispy-occur" "")
                        (,(cae-keyboard-kbd "o") "lispy-other-mode" "")
                        (,(cae-keyboard-kbd "J") "lispy-outline-next" "")
                        (,(cae-keyboard-kbd "K") "lispy-outline-prev" "")
                        (,(cae-keyboard-kbd "P") "lispy-paste" "")
                        (,(cae-keyboard-kbd "l") "lispy-right" "")
                        (,(cae-keyboard-kbd "I") "lispy-shifttab" "")
                        (">" "lispy-slurp" "")
                        ("SPC" "lispy-space" "")
                        (,(cae-keyboard-kbd "xB") "lispy-store-region-and-buffer" "")
                        (,(cae-keyboard-kbd "u") "lispy-undo" "")
                        (,(cae-keyboard-kbd "k") "lispy-up" "")
                        (,(cae-keyboard-kbd "v") "lispy-view" "")
                        (,(cae-keyboard-kbd "V") "lispy-visit" "")
                        (,(cae-keyboard-kbd "W") "lispy-widen" "")
                        (,(cae-keyboard-kbd "D") "pop-tag-mark" "")
                        (,(cae-keyboard-kbd "x") "see" "")
                        (,(cae-keyboard-kbd "L") "unbound" "")
                        (,(cae-keyboard-kbd "U") "unbound" "")
                        (,(cae-keyboard-kbd "X") "unbound" "")
                        (,(cae-keyboard-kbd "Y") "unbound" "")
                        (,(cae-keyboard-kbd "H") "lispy-ace-symbol-replace" "Edit")
                        (,(cae-keyboard-kbd "c") "lispy-clone" "Edit")
                        (,(cae-keyboard-kbd "C") "lispy-convolute" "Edit")
                        (,(cae-keyboard-kbd "n") "lispy-new-copy" "Edit")
                        (,(cae-keyboard-kbd "O") "lispy-oneline" "Edit")
                        (,(cae-keyboard-kbd "r") "lispy-raise" "Edit")
                        (,(cae-keyboard-kbd "R") "lispy-raise-some" "Edit")
                        ("\\" "lispy-splice" "Edit")
                        (,(cae-keyboard-kbd "S") "lispy-stringify" "Edit")
                        (,(cae-keyboard-kbd "i") "lispy-tab" "Edit")
                        (,(cae-keyboard-kbd "xj") "lispy-debug-step-in" "Eval")
                        (,(cae-keyboard-kbd "xe") "lispy-edebug" "Eval")
                        (,(cae-keyboard-kbd "xT") "lispy-ert" "Eval")
                        (,(cae-keyboard-kbd "e") "lispy-eval" "Eval")
                        (,(cae-keyboard-kbd "E") "lispy-eval-and-insert" "Eval")
                        (,(cae-keyboard-kbd "xr") "lispy-eval-and-replace" "Eval")
                        (,(cae-keyboard-kbd "p") "lispy-eval-other-window" "Eval")
                        (,(cae-keyboard-kbd "q") "lispy-ace-paren" "Move")
                        (,(cae-keyboard-kbd "z") "lispy-knight" "Move")
                        (,(cae-keyboard-kbd "s") "lispy-move-down" "Move")
                        (,(cae-keyboard-kbd "w") "lispy-move-up" "Move")
                        (,(cae-keyboard-kbd "t") "lispy-teleport" "Move")
                        (,(cae-keyboard-kbd "Q") "lispy-ace-char" "Nav")
                        (,(cae-keyboard-kbd "-") "lispy-ace-subword" "Nav")
                        (,(cae-keyboard-kbd "a") "lispy-ace-symbol" "Nav")
                        (,(cae-keyboard-kbd "b") "lispy-back" "Nav")
                        (,(cae-keyboard-kbd "d") "lispy-different" "Nav")
                        (,(cae-keyboard-kbd "f") "lispy-flow" "Nav")
                        (,(cae-keyboard-kbd "F") "lispy-follow" "Nav")
                        (,(cae-keyboard-kbd "g") "lispy-goto" "Nav")
                        (,(cae-keyboard-kbd "xb") "lispy-bind-variable" "Refactor")
                        (,(cae-keyboard-kbd "xf") "lispy-flatten" "Refactor")
                        (,(cae-keyboard-kbd "xc") "lispy-to-cond" "Refactor")
                        (,(cae-keyboard-kbd "xd") "lispy-to-defun" "Refactor")
                        (,(cae-keyboard-kbd "xi") "lispy-to-ifs" "Refactor")
                        (,(cae-keyboard-kbd "xl") "lispy-to-lambda" "Refactor")
                        (,(cae-keyboard-kbd "xu") "lispy-unbind-variable" "Refactor")
                        (,(cae-keyboard-kbd "M") "lispy-multiline" "Other")
                        (,(cae-keyboard-kbd "xh") "lispy-describe" "Other")
                        (,(cae-keyboard-kbd "m") "lispy-mark-list" "Other"))))
        (eval
         (append
          '(defhydra cae-lispy-cheat-sheet (:hint nil :foreign-keys run)
             ("<f6>" nil "Exit" :exit t))
          (cl-loop for x in bindings
                   unless (string= "" (elt x 2))
                   collect
                   (list (car x)
                         (intern (elt x 1))
                         (when (string-match "lispy-\\(?:eval-\\)?\\(.+\\)"
                                             (elt x 1))
                           (match-string 1 (elt x 1)))
                         :column
                         (elt x 2)))))
        (define-key lispy-mode-map (kbd "<f6>") #'cae-lispy-cheat-sheet/body)))
    (map! :map lispy-mode-map-lispy
          "M-m" nil                  ; formerly `lispy-mark-symbol'. I prefer to
                                        ; keep the default `M-m' and `C-a'
                                        ; keybindings.
          "C-a" nil                   ; formerly `lispy-move-beginning-of-line'.
          "C-e" nil                   ; formerly `lispy-move-end-of-line'.
                                        ; Doom's default `C-e' is better.
          "M-j" nil                     ; formerly `lisp-split'.
          "M-r" #'lispy-raise-sexp
          "M-R" #'lispy-raise-some
          "M-S" #'lispy-split
          "M-C" #'lispy-convolute-sexp)
    (when (modulep! :editor multiple-cursors)
      (after! multiple-cursors-core
        (add-to-list 'mc/unsupported-minor-modes 'lispy-mode)))))
