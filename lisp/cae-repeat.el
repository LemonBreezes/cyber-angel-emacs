;;; ~/.doom.d/lisp/cae-repeat.el -*- lexical-binding: t; -*-

(defun other-window-previous (count &optional all-frames)
  "Select the previous window."
  (interactive "p\nP")
  (other-window (- count) all-frames t))

(map! "C-x O" #'other-window-previous)

;; Uses special keys from my esoteric keyboard layout
(define-repeat-map other-window
  ("o" other-window
   "O" other-window-previous
   "q" quit-window
   "p" +popup/other
   "K" doom/kill-this-buffer-in-all-windows
   ;; Number row keys
   "!" delete-other-windows
   "@" split-window-below
   ";" split-window-right
   ")" +workspace/close-window-or-workspace))

(define-repeat-map isearch-repeat
  ("s" isearch-repeat-forward
   "r" isearch-repeat-backward))

(define-repeat-map winner
  ("u" winner-undo
   "r" winner-redo))

(define-repeat-map pop-global-mark
  ("C-@" pop-global-mark))

(defun cae-repeat-ignore-when-hydra-active-a ()
  (and (featurep 'hydra) hydra-curr-map))
(advice-add #'repeat-post-hook :before-until
            #'cae-repeat-ignore-when-hydra-active-a)

(autoload 'embark-verbose-indicator "embark")
(autoload 'which-key--create-buffer-and-show "which-key")

;; Karthink might have a package for defining repeat maps.

(add-hook 'doom-first-input-hook #'repeat-mode)
(after! which-key
  (require 'repeat-help)
  (setq! repeat-help-popup-type 'which-key
          repeat-help-auto nil)
  (repeat-help-mode +1))
(map! :map help-map "C-r" #'describe-repeat-maps)
