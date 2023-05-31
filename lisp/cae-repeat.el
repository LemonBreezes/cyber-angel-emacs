;;; ~/.doom.d/lisp/cae-repeat.el -*- lexical-binding: t; -*-

(map! "C-x O" #'other-window-previous)

;; Uses special keys from my esoteric keyboard layout
(define-repeat-map other-window
  ("o" other-window
   "O" other-window-previous
   "q" quit-window
   "p" +popup/other
   "K" kill-buffer-and-window
   "<left>" previous-buffer
   "<right>" next-buffer
   "u" winner-undo
   "r" winner-redo
   ;; Number row keys
   "!" delete-other-windows
   "@" split-window-below
   ";" split-window-right
   ")" +workspace/close-window-or-workspace)
  (:exit "f" find-file))

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

(use-package! repeat
  :init
  (add-hook 'doom-first-input-hook #'repeat-mode)
  :config
  (after! which-key
    (require 'repeat-help)
    (setq repeat-help-popup-type 'which-key
          repeat-help-auto nil)
    (repeat-help-mode +1))
  (setq repeat-exit-timeout 0.5)
  (map! :map help-map "C-r" #'describe-repeat-maps))
