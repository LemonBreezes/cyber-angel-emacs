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

;; Bind `better-jumper-jump-forward' to TAB in terminal Emacs since can't dicern
;; between TAB and C-i there.
(unless (cae-display-graphic-p)
  (after! evil
    (define-key evil-motion-state-map (kbd "TAB") nil)
    (add-hook! (prog-mode conf-mode text-mode)
      (evil-local-set-key 'motion (kbd "TAB") #'better-jumper-jump-forward))))

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

;; Vim normally binds this to `C-e' but the Emacs `C-a'/`C-e' are more too
;; ubiquitous to give up.
(map! :i "C-S-e" #'evil-copy-from-below)

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

;; Gotta have my Emacs bindings.
(map! :leader "k" #'ctl-x-map)

;; Define help better keybinding help commands for Evil.
(map! :n "C-M-?" #'cae-which-key-show-state-keymap)
(after! help
  (map! :map help-map
        "bn" #'cae-which-key-show-state-keymap
        "bh" #'cae-embark-state-bindings))

;; Use `C-a' to append in a more generalized context.
(map! :n "C-a" #'cae-evil-append-buffer-or-code)

;; I prefer to not continue comments with o/O in Evil.
(advice-remove #'evil-open-below #'+evil--insert-newline-below-and-respect-comments-a)
(advice-remove #'evil-open-above #'+evil--insert-newline-above-and-respect-comments-a)

;; Define a leader key for switching to popup windows.
(after! evil
  (unless (lookup-key evil-window-map "e")
    (map! :map evil-window-map
          "e" #'+popup/other))
  (unless (lookup-key evil-window-map "~")
    (map! :map evil-window-map
          "~" #'+popup/raise)))

;; Isearch is better in `Info-mode'
(after! evil
  (unless (eq evil-search-module 'isearch)
    (after! info
      (map! :map Info-mode-map
            :m "/" #'isearch-forward-regexp
            :m "?" #'isearch-backward-regexp))))

(when (modulep! :editor evil +hybrid)
  ;; Return Isearch to Evil
  (map! :m "C-r" #'isearch-backward
        :m "C-s" #'isearch-forward
        :n "C-r" nil
        :n "U" #'evil-redo)

  ;; Use Emacs keybindings in Evil insert state.
  ;; We do not disable Omnicompletion keybindings though.
  (setq evil-disable-insert-state-bindings nil)
  (after! evil
    (define-key evil-insert-state-map [escape] 'evil-normal-state)
    (define-key evil-insert-state-map
      (read-kbd-macro evil-toggle-key) 'evil-emacs-state))
  (define-key! :keymaps +default-minibuffer-maps
    "C-a" nil
    "C-r" nil
    "C-u" nil
    "C-v" nil
    "C-w" nil
    "C-z" nil))
(define-key! :keymaps +default-minibuffer-maps
  "C-l"    #'cae-yank-word-to-minibuffer)
(after! vertico-map
  (map! :map vertico-map
        "C-l" #'cae-yank-word-to-minibuffer))
(after! isearch
  (map! :map isearch-mode-map
        "C-w" nil
        "C-S-w" #'isearch-yank-word-or-char))
;; These keybindings are so garbage. I can't believe Doom Emacs has them.
;; Use C-n/C-p in the minibuffer instead.
(define-key! :keymaps +default-minibuffer-maps
  "C-j"    nil
  "C-k"    nil
  "C-S-j"  nil
  "C-S-k"  nil)
(define-key! read-expression-map
  "C-j" nil
  "C-k" nil)
(define-key! :states 'insert :keymaps +default-minibuffer-maps
  "C-j"    nil
  "C-k"    nil)

(map! :n "zE" #'cae-evil-edit-indirect)

(after! evil-easymotion
  (map! :map evilem-map
        "RET" #'cae-avy-embark-act-on-region
        "r" #'cae-avy-rotate
        "TAB" #'tabgo))

(map! :prefix "g"
      :m "[" #'backward-page
      :m "]" #'cae-forward-page)

(after! evil-snipe
  (setq evil-snipe-scope 'whole-visible))
(setopt evil-ex-substitute-global t
        evil-move-cursor-back nil
        evil-vsplit-window-right t
        evil-kill-on-visual-paste nil
        evil-split-window-below t
        evil-v$-excludes-newline t)

;; I like for scrolling by page to move the point to the beginning or end of the
;; buffer as is typical for other editors.
(defun cae-evil-scroll-page-a (direction oldfun args)
  (apply oldfun args)
  (cond ((eq direction 'down)
         (when (eq (pos-eol) (point-max))
           (goto-char (point-max))))
        ((eq direction 'up)
         (when (eq (pos-bol) (point-min))
           (goto-char (point-min))))))

(defun cae-evil-scroll-page-down-a (oldfun &rest args)
  (cae-evil-scroll-page-a 'down oldfun args))

(defun cae-evil-scroll-page-up-a (oldfun &rest args)
  (cae-evil-scroll-page-a 'up oldfun args))

(cae-advice-add #'evil-scroll-page-down :around #'cae-evil-scroll-page-down-a)
(cae-advice-add #'evil-scroll-page-up :around #'cae-evil-scroll-page-up-a)

;; Use `C-d' to send EOF in comint buffers.
(after! comint
  (map! :map comint-mode-map
        :i "C-d" #'cae-comint-delchar-or-maybe-eof))

;; Evil Collection setup.
(let ((blacklist (append (when (modulep! :config default)
                           (list doom-leader-key doom-localleader-key
                                 doom-leader-alt-key))
                         (when (modulep! :editor multiple-cursors)
                           '("gz"))
                         ;;(when (modulep! :tools lookup)
                         ;;  '("gd" "gf" "K"))
                         ;; Hopefuly setting `evil-collection-want-find-usages-bindings'
                         ;; to nil is enough.
                         ;; (when (modulep! :tools eval)
                         ;; '("gr" "gR"))
                         '("<escape>" "[" "]" "<backspace>" "DEL"))))
  (setq evil-collection-key-blacklist blacklist)
  (after! evil-collection
    (setq evil-collection-key-blacklist blacklist
          evil-collection-want-find-usages-bindings nil)
    (map! :n "]p" #'cae-unimpaired-paste-below
          :n "[p" #'cae-unimpaired-paste-above
          :m "[6" #'cae-unimpaired-b64-encode
          :m "]6" #'cae-unimpaired-b64-decode)))

(after! comint
  (map! :map comint-mode-map
        :n "C-p" nil
        :n "C-n" nil))
(after! eshell
  (map! :map eshell-mode-map
        :n "C-p" nil
        :n "C-n"))
(after! vterm
  (map! :map vterm-mode-map
        [remap evil-paste-pop] #'vterm-yank-pop))

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
       :i "C-c" #'minuet-show-suggestion
       "y" #'minuet-complete-with-minibuffer))


;; TODO Fix this to work with `consult-yasnippet'.
;;(cae-defadvice! cae-evil-insert-state-a (&rest _)
;;  :after #'yas-expand-snippet
;;  (call-interactively #'evil-insert))

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
  (key-chord-define-global "jj" #'evilem-motion-next-line)
  (key-chord-define-global "kk" #'evilem-motion-previous-line)
  (after! transient
    (map! :map transient-base-map
          ;; Extend `evil-escape' to quit transients.
          "<key-chord>" #'transient-quit-all)))

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
