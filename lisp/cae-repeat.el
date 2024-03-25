  ;;; ~/.doom.d/lisp/cae-repeat.el -*- lexical-binding: t; -*-

(use-package! repeat
  :defer t :init
  (advice-add #'repeat-mode :around #'cae-shut-up-a)
  (add-hook 'doom-first-input-hook #'repeat-mode)
  :config
  (map! :map help-map "C-r" #'describe-repeat-maps)
  (setq repeat-exit-key "TAB"
        repeat-check-key t
        repeat-echo-mode-line-string nil)

  (define-repeat-map isearch-repeat
    ("s" isearch-repeat-forward
     "r" isearch-repeat-backward))

  (eval
   `(define-repeat-map workspace-switch
      (,(cae-keyboard-kbd "1") +workspace/switch-to-0
       ,(cae-keyboard-kbd "2") +workspace/switch-to-1
       ,(cae-keyboard-kbd "3") +workspace/switch-to-2
       ,(cae-keyboard-kbd "4") +workspace/switch-to-3
       ,(cae-keyboard-kbd "5") +workspace/switch-to-4
       ,(cae-keyboard-kbd "6") +workspace/switch-to-5
       ,(cae-keyboard-kbd "7") +workspace/switch-to-6
       ,(cae-keyboard-kbd "8") +workspace/switch-to-7
       ,(cae-keyboard-kbd "9") +workspace/switch-to-8
       ,(cae-keyboard-kbd "0") cae-workspace-switch-to-9
       ,(cae-keyboard-kbd "\\") cae-workspace-switch-to-10
       ,(cae-keyboard-kbd "z") +workspace/switch-to-final
       ,(cae-keyboard-kbd "[") +workspace/switch-left
       ,(cae-keyboard-kbd "]") +workspace/switch-right
       "1" +workspace/switch-to-0
       "2" +workspace/switch-to-1
       "3" +workspace/switch-to-2
       "4" +workspace/switch-to-3
       "5" +workspace/switch-to-4
       "6" +workspace/switch-to-5
       "7" +workspace/switch-to-6
       "8" +workspace/switch-to-7
       "9" +workspace/switch-to-8
       "0" cae-workspace-switch-to-9
       "\\" cae-workspace-switch-to-10
       "z" +workspace/switch-to-final
       "[" +workspace/switch-left
       "]" +workspace/switch-right
       "`" +workspace/other))
   t)

  (define-repeat-map winum
    ("0" winum-select-window-0-or-10
     "1" winum-select-window-1
     "2" winum-select-window-2
     "3" winum-select-window-3
     "4" winum-select-window-4
     "5" winum-select-window-5
     "6" winum-select-window-6
     "7" winum-select-window-7
     "8" winum-select-window-8
     "9" winum-select-window-9))

  (define-repeat-map cae-dired-jump
    ("-" cae-dired-jump))

  (define-repeat-map expand-region-improved
    ("=" eri/expand-region
     "-" eri/contract-region))

  (define-repeat-map avy-rotate
    ("r" cae-avy-rotate))

  (define-repeat-map cae-jump-to-random-line-end
    ("j" cae-jump-to-random-line-end))

  (define-repeat-map git-rebase-move-line
    ("j" git-rebase-move-line-down)
    ("k" git-rebase-move-line-up))

  (define-repeat-map org-priority
    ("u" org-priority-up
     "d" org-priority-down))

  (define-repeat-map emms-next-prev
    ("j" emms-next
     "k" emms-previous))

  (define-repeat-map org-ai-kill-region-at-point
    ("DEL" org-ai-kill-region-at-point))
  (define-repeat-map cae-org-ai-kill-region-at-point
    ("DEL" cae-org-ai-kill-region-at-point))

  ;; Currently part of `other-window' repeat map
  (define-repeat-map winner
    ("u" winner-undo
     "r" winner-redo
     "U" winner-redo))

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

  (define-repeat-map multiple-cursors-mark
    ("n" mc/mark-next-like-this
     "p" mc/mark-previous-like-this
     "N" mc/unmark-next-like-this
     "P" mc/unmark-previous-like-this))

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

  (when (modulep! :editor evil)
    (define-repeat-map evil-window-next
      ("w" evil-window-next
       "W" evil-window-prev))

    (define-repeat-map evil-scroll
      ("d" evil-scroll-down
       "u" evil-scroll-up))

    (define-repeat-map evil-scroll-page
      ("f" evil-scroll-page-down
       "b" evil-scroll-page-up))

    (define-repeat-map evil-scroll-line
      ("e" evil-scroll-line-down
       "y" evil-scroll-line-up))

    (define-repeat-map evil-complete-line
      ("C-p" evil-complete-previous-line
       "C-n" evil-complete-next-line))

    (define-repeat-map evil-paste-pop
      ("n" evil-paste-pop-next
       "p" evil-paste-pop))

    (define-repeat-map tab-bar-switch-next-tab
      ("t" tab-bar-switch-to-next-tab
       "T" tab-bar-switch-to-prev-tab)))

  (defun cae-repeat-ignore-when-hydra-active-a ()
    (bound-and-true-p hydra-curr-map))

  (advice-add #'repeat-post-hook :before-until
              #'cae-repeat-ignore-when-hydra-active-a)

  (after! outline
    (map! :map outline-navigation-repeat-map
          "RET" #'outline-toggle-children)
    (put #'outline-toggle-children 'repeat-map 'outline-navigation-repeat-map))

  (autoload 'embark-verbose-indicator "embark")
  (autoload 'which-key--create-buffer-and-show "which-key")

  ;; This is so that my repeat maps are reloaded when I save this file.
  (when cae-config-finished-loading
    (ignore-errors (repeat-mode -1))
    (ignore-errors (repeat-mode 1))))
