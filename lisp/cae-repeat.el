;;; ~/.doom.d/lisp/cae-repeat.el -*- lexical-binding: t; -*-

(map! "C-x O" #'other-window-previous)

(use-package! repeat
  :init
  (add-hook 'doom-first-input-hook #'repeat-mode)
  :config
  (map! :map help-map "C-r" #'describe-repeat-maps)
  (setq repeat-exit-key "TAB")

  (eval
   `(define-repeat-map other-window
      ("o" other-window
       "O" other-window-previous
       ,@(when (modulep! :ui popup)
           '("p" +popup/other))
       "0" delete-window
       "1" doom/window-maximize-buffer
       "2" split-window-below
       "3" split-window-right
       ,(cae-keyboard-kbd "0") delete-window
       ,(cae-keyboard-kbd "1") doom/window-maximize-buffer
       ,(cae-keyboard-kbd "2") split-window-below
       ,(cae-keyboard-kbd "3") split-window-right
       "M-o" ace-swap-window))
   t)

  (define-repeat-map isearch-repeat
    ("s" isearch-repeat-forward
     "r" isearch-repeat-backward))

  (eval
   `(define-repeat-map tab-next
      ("o" tab-next
       "O" tab-previous
       ,(cae-keyboard-kbd "0") tab-close
       ,(cae-keyboard-kbd "1") tab-close-other
       ,(cae-keyboard-kbd "2") tab-new
       "0" tab-close
       "1" tab-close-other
       "2" tab-new))
   t)

  ;;Currently part of `other-window' repeat map
  ;;(define-repeat-map winner
  ;;  ("u" winner-undo
  ;;   "r" winner-redo))

  (define-repeat-map cae-buffer-navigation-repeat-map
    ("<left>" cae-dired-previous-buffer
     "<right>" cae-dired-next-buffer))

  (define-repeat-map set-mark-command
    ("SPC" set-mark-command))

  (define-repeat-map pop-global-mark
    ("C-@" pop-global-mark))

  ;; Now I can scroll with `v' and reverse directions with `V'! Amazing!
  (define-repeat-map scroll-up-command
    ("v" scroll-up-command)
    (:exit "V" scroll-down-command))
  (define-repeat-map scroll-down-command
    ("v" scroll-down-command)
    (:exit "V" scroll-up-command))

  (define-repeat-map scroll-other-window
    ("v" scroll-other-window)
    (:exit "V" scroll-other-window-down))
  (define-repeat-map scroll-other-window-down
    ("v" scroll-other-window-down)
    (:exit "V" scroll-other-window))

  (define-repeat-map vertico-scroll-up
    ("v" vertico-scroll-up)
    (:exit "V" vertico-scroll-down))
  (define-repeat-map vertico-scroll-down
    ("v" vertico-scroll-down)
    (:exit "V" vertico-scroll-up))

  (eval
   `(define-repeat-map workspace-switch
      (,(cae-keyboard-kbd "0") +workspace/switch-to-final
       ,(cae-keyboard-kbd "1") +workspace/switch-to-0
       ,(cae-keyboard-kbd "2") +workspace/switch-to-1
       ,(cae-keyboard-kbd "3") +workspace/switch-to-2
       ,(cae-keyboard-kbd "4") +workspace/switch-to-3
       ,(cae-keyboard-kbd "5") +workspace/switch-to-4
       ,(cae-keyboard-kbd "6") +workspace/switch-to-5
       ,(cae-keyboard-kbd "7") +workspace/switch-to-6
       ,(cae-keyboard-kbd "8") +workspace/switch-to-7
       ,(cae-keyboard-kbd "9") +workspace/switch-to-8
       "0" +workspace/switch-to-final
       "1" +workspace/switch-to-0
       "2" +workspace/switch-to-1
       "3" +workspace/switch-to-2
       "4" +workspace/switch-to-3
       "5" +workspace/switch-to-4
       "6" +workspace/switch-to-5
       "7" +workspace/switch-to-6
       "8" +workspace/switch-to-7
       "9" +workspace/switch-to-8))
   t)

  (defun cae-repeat-ignore-when-hydra-active-a ()
    (bound-and-true-p hydra-curr-map))

  (advice-add #'repeat-post-hook :before-until
              #'cae-repeat-ignore-when-hydra-active-a)

  (autoload 'embark-verbose-indicator "embark")
  (autoload 'which-key--create-buffer-and-show "which-key")

  ;; This is so that my repeat maps are reloaded when I save this file.
  (when cae-config-finished-loading
    (ignore-errors (repeat-mode -1))
    (ignore-errors (repeat-mode +1))))
