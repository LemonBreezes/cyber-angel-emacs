;;; lisp/cae-evil.el -*- lexical-binding: t; -*-

(setq +evil-want-move-window-to-wrap-around t)

;; I use a split keyboard and map backspace to my left thumb key.
;;(lookup-key evil-normal-state-map doom-localleader-key)
(after! evil-easymotion
  (map! :map evilem-map
        "DEL" (lookup-key evilem-map "\s")))
(after! evil-collection
  (map! :n "] DEL" #'+evil/insert-newline-below
        :n "[ DEL" #'+evil/insert-newline-above))

;; Vim normally binds this to `C-e' but the Emacs `C-a'/`C-e' are more too
;; ubiquitous to give up.
(map! :i "C-S-e" #'evil-copy-from-below)

;; Restore Emacs keybindings which Doom overrides. `expand-region-fast-keys'
;; and `C-x C-=' make these keybindings redundant.
(map! "C--" #'negative-argument
      "M--" #'negative-argument
      "M-=" #'count-words-region)

;; Unbind text scaling functions. We use C-x C-=.
(map! :n "C--" nil
      :n "C-+" nil
      :n "C-=" nil
      :n "M-C-=" nil
      :n "M-C--" nil)

;; Bind `better-jumper-jump-forward' to TAB in terminal Emacs since can't dicern
;; between TAB and C-i there.
(unless (cae-display-graphic-p)
  (after! evil
    (define-key evil-motion-state-map (kbd "TAB") nil)
    (add-hook! (prog-mode conf-mode text-mode)
      (evil-local-set-key 'motion (kbd "TAB") #'better-jumper-jump-forward))))

;; I never use `gt' and `gT' for workspace navigation.
(map! :n "gt" #'tab-bar-switch-to-next-tab
      :n "gT" #'tab-bar-switch-to-prev-tab)
(after! magit
  (map! :map magit-status-mode-map
        :nv "gt" #'tab-bar-switch-to-next-tab))

;; For some reason this command is bound differently in my Emacs!
(after! magit
  (map! :map magit-status-mode-map
        :nv "gz" #'magit-jump-to-stashes))

;; Use `C-a' to append in a more generalized context.
(map! :n "C-a" #'cae-evil-append-buffer-or-code)

;;; Chords

(after! evil-escape
  (remove-hook 'evil-escape-inhibit-functions #'+evil-inhibit-escape-in-minibuffer-fn))
(cae-defadvice! cae-evil-escape-fix-for-restore-point ()
  :after #'evil-escape-pre-command-hook
  (let ((esc-func (evil-escape-func)))
    (when (eq this-command esc-func)
      (setq real-this-command esc-func))))

(use-package! key-chord
  :defer t :init
  (cae-advice-add #'evil-escape-mode :override #'key-chord-mode)
  :config
  (setq key-chord-two-keys-delay 0.2)
  (setq key-chord-typing-speed-threshold 0.15)
  (setq key-chord-one-key-min-delay 0.06)
  (setq key-chord-typing-reset-delay 0.45)
  (setq key-chord-typing-detection t)
  (setq key-chord-in-macros nil)
  (autoload 'evil-escape "evil-escape" nil t)
  (key-chord-define-global "jk" #'evil-escape)
  ;; Make the chords more flexible in case I need to type them, though rare.
  (key-chord-define-global "hh" #'cae-evil-avy-goto-char-or-self-insert)
  (key-chord-define-global "jj" #'cae-evil-next-line-or-quick-jump)
  (key-chord-define-global "kk" #'cae-evil-previous-line-or-quick-insert)
  (after! transient
    (map! :map transient-base-map
          "<escape>" #'transient-quit-one
          ;; Extend `evil-escape' to quit transients.
          "<key-chord>" #'transient-quit-one)))

;; Add rotation commands
(use-package! parrot-rotate
  :defer t :init
  (map! :n "]r" #'parrot-rotate-next-word-at-point
        :n "[r" #'parrot-rotate-prev-word-at-point)
  :config
  (after! parrot-rotate
    (setq parrot-rotate-highlight-after-rotation t
          parrot-rotate-animate-after-rotation nil
          parrot-rotate-start-bound-regexp "[\]\[[:space:](){}<>]"
          parrot-rotate-end-bound-regexp "[\]\[[:space:](){}<>]")
    (add-to-list 'parrot-rotate-dict '(:rot ("add-hook" "remove-hook")))
    (add-to-list 'parrot-rotate-dict '(:rot ("add-hook!" "remove-hook!")))
    (add-to-list 'parrot-rotate-dict '(:rot ("setq-hook!" "unsetq-hook!")))
    (add-to-list 'parrot-rotate-dict '(:rot ("Yes" "No")))
    (add-to-list 'parrot-rotate-dict '(:rot ("nil" "t")))
    (add-to-list 'parrot-rotate-dict '(:rot ("-1" "+1")))
    (add-to-list 'parrot-rotate-dict '(:rot ("when" "unless")))
    (add-to-list 'parrot-rotate-dict '(:rot ("add-to-list" "remove-from-list")))
    (add-to-list 'parrot-rotate-dict '(:rot ("advice-add" "advice-remove")))
    (add-to-list 'parrot-rotate-dict '(:rot ("cae-defadvice!" "uncae-defadvice!")))
    (add-to-list 'parrot-rotate-dict '(:rot ("cae-keyboard-remap"
                                             "cae-keyboard-remap-to-strings"
                                             "cae-keyboard-strings")))
    (add-to-list 'parrot-rotate-dict '(:rot ("kbd" "cae-keyboard-kbd")))
    (add-to-list 'parrot-rotate-dict '(:rot ("+log" "message")))
    (add-to-list 'parrot-rotate-dict '(:rot ("backtrace!" "unbacktrace!")))
    (add-to-list 'parrot-rotate-dict '(:rot ("enabled" "disabled")))))

;;Local Variables:
;;eval: (unless (modulep! :editor evil) (remove-hook 'write-file-functions #'eval-buffer t))
;;End:
