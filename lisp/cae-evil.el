;;; lisp/cae-evil.el -*- lexical-binding: t; -*-


;; Restore Emacs keybindings which Doom overrides. `expand-region-fast-keys'
;; and `C-x C-=' make these keybindings redundant.
(map! "C--" #'negative-argument
      "M--" #'negative-argument
      "M-=" #'count-words-region)

(define-key! :keymaps +default-minibuffer-maps
  [escape] #'abort-recursive-edit)

;; Unbind text scaling functions. We use C-x C-=.
(map! :n "C--" nil
      :n "C-+" nil
      :n "C-=" nil
      :n "M-C-=" nil
      :n "M-C--" nil)


;; Isearch is better in `Info-mode'
(map! :map Info-mode-map
      :m "/" #'isearch-forward-regexp
      :m "?" #'isearch-backward-regexp)

;; Return Isearch to Evil
(map! :m "C-r" #'isearch-backward
      :m "C-s" #'isearch-forward
      :n "U" #'evil-redo)


;; Use Emacs keybindings in Evil insert state.
(setq! evil-disable-insert-state-bindings t)
(define-key! :keymaps +default-minibuffer-maps
  "C-a" nil
  "C-r" nil
  "C-u" nil
  "C-v" nil
  "C-w" nil
  "C-z" nil)
