;;; lisp/cae-evil.el -*- lexical-binding: t; -*-

;; I use a split keyboard and map backspace to my left thumb key.
(when (string= doom-leader-key "SPC")
  (map! :nmv "DEL" doom-leader-map))

;; This allows us to circumvent the `evil-collection' keybindings.
(setq evil-default-state 'insert)

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
(setq evil-disable-insert-state-bindings nil
      evil-insert-state-map (make-sparse-keymap))
(define-key evil-insert-state-map [escape] 'evil-normal-state)
(define-key evil-insert-state-map
  (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
(define-key! :keymaps +default-minibuffer-maps
  "C-a" nil
  "C-r" nil
  "C-u" nil
  "C-v" nil
  "C-w" nil
  "C-z" nil)

;; TODO Undefine the C-j, C-k, etc keys.

(map! :prefix "g"
      :m "[" #'backward-page
      :m "]" #'cae-forward-page)

;; Unmap `C-d` and `C-u` in Evil since we use the `<prior>` and `<next>` keys
;; instead.
(map! :m "C-d" nil
      :m "C-u" nil)
