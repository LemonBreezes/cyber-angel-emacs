;;; ~/.doom.d/lisp/cae-repeat.el -*- lexical-binding: t; -*-

(map! "C-x O" #'other-window-previous)

(use-package! repeat
  :init
  (add-hook 'doom-first-input-hook #'repeat-mode)
  :config
  (map! :map help-map "C-r" #'describe-repeat-maps)
  (setq repeat-exit-timeout nil)

  (define-repeat-map other-window
    ("o" other-window
     "O" other-window-previous))

  (define-repeat-map isearch-repeat
    ("s" isearch-repeat-forward
     "r" isearch-repeat-backward))

  (define-repeat-map winner
    ("u" winner-undo
     "r" winner-redo))

  (define-repeat-map cae-buffer-navigation-repeat-map
    ("<left>" cae-previous-buffer
     "<right>" cae-next-buffer))

  (define-repeat-map set-mark-command
    ("SPC" set-mark-command))

  (define-repeat-map pop-global-mark
    ("C-@" pop-global-mark))

  ;; Now I can scroll with `v' and reverse directions with `V'!! Amazing!
  (define-repeat-map scroll-up-command
    ("v" scroll-up-command)
    (:exit "V" scroll-down-command))
  (define-repeat-map scroll-down-command
    ("v" scroll-down-command)
    (:exit "V" scroll-up-command))

  (defun cae-repeat-ignore-when-hydra-active-a ()
    (and (featurep 'hydra) hydra-curr-map))

  (advice-add #'repeat-post-hook :before-until
              #'cae-repeat-ignore-when-hydra-active-a)

  (autoload 'embark-verbose-indicator "embark")
  (autoload 'which-key--create-buffer-and-show "which-key"))

;; So that my repeat maps are reloaded when I change them.
(when cae-config-finished-loading
  (ignore-errors (repeat-mode -1))
  (ignore-errors (repeat-mode +1)))
