;;; lisp/cae-evil.el -*- lexical-binding: t; -*-

(setq +evil-want-move-window-to-wrap-around t)

;; Gotta have my Emacs bindings.
(map! :leader "k" #'ctl-x-map)

;; I use a split keyboard and map backspace to my left thumb key.
;;(lookup-key evil-normal-state-map doom-localleader-key)
(after! evil-easymotion
  (map! :map evilem-map
        "DEL" (lookup-key evilem-map "\s")))
(after! evil-collection
  (map! :n "] DEL" #'+evil/insert-newline-below
        :n "[ DEL" #'+evil/insert-newline-above))

;; Define help better keybinding help commands for Evil.
(map! :n "C-M-?" #'cae-which-key-show-state-keymap)
(after! help
  (map! :map help-map
        "bn" #'cae-which-key-show-state-keymap
        "bh" #'cae-embark-state-bindings))

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

;; Isearch is better in `Info-mode'
(after! evil
  (unless (eq evil-search-module 'isearch)
    (after! info
      (map! :map Info-mode-map
            :m "/" #'isearch-forward-regexp
            :m "?" #'isearch-backward-regexp))))

;; For some reason this command is bound differently in my Emacs!
(after! magit
  (map! :map magit-status-mode-map
        :nv "gz" #'magit-jump-to-stashes))

(map! :n "C-a" #'cae-evil-append-buffer-or-code)
(map! :n "zE" #'cae-evil-edit-indirect)
(map! :prefix "g"
      :m "[" #'backward-page
      :m "]" #'cae-forward-page)

(after! evil-easymotion
  (map! :map evilem-map
        "RET" #'cae-avy-embark-act-on-region
        "r" #'cae-avy-rotate
        "TAB" #'tabgo))


(after! evil-org
  (map! :map evil-org-mode-map
        :i "C-u" #'cae-evil-org-delete-back-to-indentation)
  (map! :map org-mode-map
        :g "M-RET" #'cae-evil-org-insert-heading
        :g "M-S-RET" #'cae-evil-org-insert-todo-heading
        :g "M-<return>" #'cae-evil-org-insert-heading
        :g "M-S-<return>" #'cae-evil-org-insert-todo-heading))
(map! :leader
      (:prefix "b"
       :desc "New empty Org buffer" "o" #'cae-evil-buffer-org-new))

(after! vterm
  (map! :map vterm-mode-map
        :localleader "e" #'vterm-send-escape))

(unless evil-disable-insert-state-bindings
  (when (modulep! :completion corfu)
    (define-key!
      :keymaps (append +default-minibuffer-maps
                       (when (modulep! :editor evil +everywhere)
                         '(evil-ex-completion-map)))
      "C-x C-f"  #'cape-file
      "C-x s"    #'cape-dict
      "C-x e"    #'cae-cape-elisp-capf
      "C-x C-s"  #'yasnippet-capf
      "C-x C-o"  #'completion-at-point
      "C-x C-n"  #'cape-dabbrev
      "C-x C-p"  #'+corfu/dabbrev-this-buffer)
    (map! :prefix "C-x"
          :i "e"   #'cae-cape-elisp-capf)))
(map! :prefix "C-x"                     ; `ctl-x-map' did not work here somehow.
      (:when (modulep! :cae ai +copilot)
        :i "C-c" #'copilot-complete)
      (:when (modulep! :cae ai -copilot)
        :i "C-c" #'minuet-show-suggestion))

;; Use `C-d' to send EOF in comint buffers.
(after! comint
  (map! :map comint-mode-map
        :i "C-d" #'cae-comint-delchar-or-maybe-eof))
;; Allow me to paste pop.
(after! comint
  (map! :map comint-mode-map
        :n "C-p" nil
        :n "C-n" nil))
(after! eshell
  (map! :map eshell-mode-map
        :n "C-p" nil
        :n "C-n" nil))
(after! vterm
  (map! :map vterm-mode-map
        [remap evil-paste-pop] #'vterm-yank-pop))

;; Combine with `cursor-in-non-selected-windows'.
(define-key! :keymaps cae-default-minibuffer-maps
  "C-l"    #'cae-yank-word-to-minibuffer)


(after! isearch
  (map! :map isearch-mode-map
        "C-w" nil
        "C-S-w" #'isearch-yank-word-or-char))

;; It'd be better to contribute bindings to `evil-collection' but this is okay.
(evil-set-initial-state #'font-lock-studio-mode 'emacs)

(defun cae-evil-mu4e-enter-insert-mode (&rest _)
  (when (eq evil-state 'normal)
    (call-interactively #'evil-append)))
(cae-advice-add #'compose-mail :after #'cae-evil-mu4e-enter-insert-mode)

(use-package! evil-owl
  :defer 5 :config
  (evil-owl-mode +1))

(after! evil-snipe
  (cl-pushnew #'calendar-mode evil-snipe-disabled-modes))

(after! evil-snipe
  (setq evil-snipe-scope 'whole-visible))
(setopt evil-ex-substitute-global t
        evil-move-cursor-back nil
        evil-vsplit-window-right t
        evil-kill-on-visual-paste nil
        evil-split-window-below t
        evil-v$-excludes-newline t)

;; I prefer to not continue comments with o/O in Evil.
(advice-remove #'evil-open-below #'+evil--insert-newline-below-and-respect-comments-a)
(advice-remove #'evil-open-above #'+evil--insert-newline-above-and-respect-comments-a)

;; OK so C-v in Evil is useless. I always use C-q instead.

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
