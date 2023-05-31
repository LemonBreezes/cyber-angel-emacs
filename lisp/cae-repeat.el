;;; ~/.doom.d/lisp/cae-repeat.el -*- lexical-binding: t; -*-

(map! "C-x O" #'other-window-previous)

(use-package! repeat
  :init
  ;; (add-hook 'doom-first-input-hook #'repeat-mode)
  (run-with-timer 2 nil #'repeat-mode +1)
  :config
  (map! :map help-map "C-r" #'describe-repeat-maps)
  (setq repeat-exit-timeout 5)

  ;; Uses special keys from my esoteric keyboard layout
  (define-repeat-map other-window
    ("o" other-window
     "O" other-window-previous))

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
  (autoload 'which-key--create-buffer-and-show "which-key"))

;; So that my repeat maps are reloaded when I change them.
(when cae-config-finished-loading
  (repeat-mode -1)
  (repeat-mode +1))
