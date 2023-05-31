;;; lisp/cae-bindings.el -*- lexical-binding: t; -*-

;; Doom should not bind leader key prefixes to keys which are not alphanumeric
;; because then they can be overwriting other packages' keybindings. As an
;; example, Org mode has `C-c !' bound to `org-time-stamp-inactive' and `C-c &'
;; bound to `org-mark-ring-goto'.
(when (modulep! :checkers syntax)
  (after! which-key
    (setq which-key-replacement-alist
          (delete '(("\\`C-c !\\'") nil . "checkers")
                  which-key-replacement-alist)))
  (after! flycheck
    (define-key flycheck-mode-map flycheck-keymap-prefix nil)
    (setq flycheck-keymap-prefix (kbd "C-c C"))
    (define-key flycheck-mode-map flycheck-keymap-prefix
      flycheck-command-map)
    (map! :leader
          (:prefix ("C" . "checkers")))))
(when (modulep! :editor snippets)
  (dolist (p (cdr (lookup-key doom-leader-map "&")))
    (cl-destructuring-bind (key . binding) p
      (define-key doom-leader-map (kbd (concat "S " (char-to-string key))) binding)))
  (after! yasnippet (define-key yas-minor-mode-map (kbd "C-c &") nil))
  (define-key doom-leader-map "&" nil)
  (after! which-key
    (setq which-key-replacement-alist
          (let ((case-fold-search nil))
            (cl-mapcar (lambda (x)
                         (when (car-safe (car x))
                           (setf (car (car x))
                                 (replace-regexp-in-string "C-c &"
                                                           "C-c S"
                                                           (car-safe (car x)))))
                         x)
                       which-key-replacement-alist)))))

;; I don't use Deft.
 (when (and (not (modulep! :ui deft))
            (eq (lookup-key doom-leader-map "nd")
                'deft))
   (define-key doom-leader-map "nd" nil))

(map! :leader
      :desc "help" "h" help-map)

;; Remove redundant `consult-history' keybinding.
(define-key!
  :keymaps (append +default-minibuffer-maps
                   (when (modulep! :editor evil +everywhere)
                     '(evil-ex-completion-map)))
  "C-s" nil)                          ;We already have `consult-history' bound
                                        ;to `M-r' and `M-s'. This way we can use
                                        ;`C-s' to search in the minibuffer.

(let ((embark-act-key "<f8>"))
    (map! embark-act-key #'embark-act
          (:when (modulep! :completion vertico)
           "C-;" nil
           (:map minibuffer-local-map
            "C-;" nil
            embark-act-key #'embark-act)))
    (eval
     `(after! embark
        (setq embark-cycle-key ,embark-act-key))
     t))

;; General keybindings.
(map! [remap backward-kill-word] #'doom/delete-backward-word ;Do not litter the kill-ring.
      [remap upcase-word] #'upcase-dwim
      [remap downcase-word] #'downcase-dwim
      [remap capitalize-word] #'capitalize-dwim
      [remap ispell-word] #'cae-ispell-word-then-abbrev
      "C-x 4 I" #'ibuffer-other-window
      [remap ibuffer] #'ibuffer-jump  ;This way
                                        ;I can do `C-x C-b =' to quickly diff a
                                        ;buffer with its file.
      "C-x _" #'shrink-window         ;Dual to `C-x ^'.
      "C-x O" #'other-window-previous ;Dual to `C-x o'.
      "C-x !" #'doom/window-enlargen
      "C-x M-o" #'ace-swap-window
      "C-x x o" #'ov-clear
      "M-Z" #'zap-up-to-char
      [C-i] #'doom/dumb-indent
      "C-S-i" #'doom/dumb-dedent
      [remap doom/backward-to-bol-or-indent] #'beginning-of-line
      [remap doom/sudo-this-file] #'cae-toggle-sudo
      (:when (modulep! :tools lookup)
       [remap xref-find-definitions] #'cae-lookup-definition-dwim)
      (:when (modulep! :completion vertico)
       [remap apropos] nil)           ;`consult-apropos' is obsolete.
      (:after man
       :map Man-mode-map
       "o" #'ace-link-man)
      (:after vertico
       (:map vertico-map
        "<prior>" #'vertico-scroll-down
        "<next>" #'vertico-scroll-up))
      (:after eww
       :map eww-mode-map
       "o" #'ace-link-eww))
(define-key resize-window-repeat-map "_" #'shrink-window)
(map! [remap delete-char] #'cae-delete-char
      ")" #'cae-insert-closing-paren)
