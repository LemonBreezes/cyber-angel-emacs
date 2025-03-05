  ;;; ~/.doom.d/lisp/cae-repeat.el -*- lexical-binding: t; -*-

(use-package! repeat
  :defer 2.0 :init
  (cae-advice-add #'repeat-mode :around #'cae-shut-up-a)
  :config
  (setq repeat-exit-key "TAB"
        repeat-check-key t
        repeat-echo-mode-line-string nil)

  (define-repeat-map isearch-repeat
    ("s" isearch-repeat-forward
     "r" isearch-repeat-backward))

  (eval
   `(define-repeat-map workspace-switch
      ("1" +workspace/switch-to-0
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
       "1" +workspace/switch-to-0
       "2" +workspace/switch-to-1
       "3" +workspace/switch-to-2
       "4" +workspace/switch-to-3
       "5" +workspace/switch-to-4
       "6" +workspace/switch-to-5
       "7" +workspace/switch-to-6
       "8" +workspace/switch-to-7
       "9" +workspace/switch-to-8
       "-" cae-workspace-switch-to-9
       ;;"=" cae-workspace-switch-to-10
       "=" +workspace/switch-to-final
       ;; Conflicts with Evil keybindings.
       ;;"[" +workspace/switch-left
       ;;"]" +workspace/switch-right
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

  (define-repeat-map cae-jump-to-random-line
    ("j" cae-jump-to-random-line))

  (define-repeat-map git-rebase-move-line
    ("j" git-rebase-move-line-down)
    ("k" git-rebase-move-line-up))

  (define-repeat-map org-priority
    ("u" org-priority-up
     "d" org-priority-down))

  (define-repeat-map emms-next-prev
    ("j" emms-next
     "k" emms-previous))

  (define-repeat-map cae-ai-org-ai-kill-region-at-point
    ("DEL" cae-ai-org-ai-kill-region-at-point))

  (define-repeat-map org-ai-kill-region-at-point
    ("DEL" org-ai-kill-region-at-point))
  (define-repeat-map cae-org-ai-kill-region-at-point
    ("DEL" cae-org-ai-kill-region-at-point))

  (define-repeat-map winner
    ("u" winner-undo
     "r" winner-redo
     "U" winner-redo))

  (define-repeat-map evil-window-width
    ("<" evil-window-decrease-width
     ">" evil-window-increase-width
     "=" balance-windows))

  (define-repeat-map evil-window-height
    ("-" evil-window-decrease-height
     "+" evil-window-increase-height
     "=" balance-windows))

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

  (define-repeat-map kill-current-buffer
    ("d" cae-kill-current-buffer))

  (when (modulep! :completion vertico)
    (define-repeat-map vertico-scroll-up
      ("v" vertico-scroll-up)
      (:exit "V" vertico-scroll-down))
    (define-repeat-map vertico-scroll-down
      ("v" vertico-scroll-down)
      (:exit "V" vertico-scroll-up)))

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
       "T" tab-bar-switch-to-prev-tab))

    (define-repeat-map evil-comint-next-prompt
      ("j" comint-next-prompt
       "k" comint-previous-prompt)))

  ;; Define repeat maps for scrolling commands.
  (define-repeat-map View-scroll-half-page-forward
    ("v" View-scroll-half-page-forward)
    (:exit "V" View-scroll-half-page-backward))
  (define-repeat-map View-scroll-half-page-backward
    ("v" View-scroll-half-page-backward)
    (:exit "V" View-scroll-half-page-forward))
  (define-repeat-map cae-View-scroll-half-page-forward-other-window
    ("v" cae-View-scroll-half-page-forward-other-window)
    (:exit "V" cae-View-scroll-half-page-backward-other-window))
  (define-repeat-map cae-View-scroll-half-page-backward-other-window
    ("v" cae-View-scroll-half-page-backward-other-window)
    (:exit "V" cae-View-scroll-half-page-forward-other-window))

  (when (modulep! :cae notifications)
    (define-repeat-map cae-ednc-toggle-notifications
      ("`" cae-ednc-toggle-notifications)))

  (when (modulep! :cae ai)
    (define-repeat-map chatgpt-shell-next-item
      ("n" chatgpt-shell-next-item
       "p" chatgpt-shell-previous-item)))

  (define-repeat-map comint-next-prompt
    ("n" comint-next-prompt
     "p" comint-previous-prompt))

  (cae-defadvice! cae-repeat-ignore-when-hydra-active-a ()
    :before-until #'repeat-post-hook
    (bound-and-true-p hydra-curr-map))

  (autoload 'embark-verbose-indicator "embark")
  (autoload 'which-key--create-buffer-and-show "which-key")
  (repeat-mode +1))

;; Local Variables:
;; after-save-hook: (lambda () (ignore-errors (repeat-mode -1)) (ignore-errors (repeat-mode 1)))
;; End:
