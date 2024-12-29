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

(map! "C-=" #'eri/expand-region)

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

(map! :n "zE" #'cae-evil-edit-indirect)

(define-key! :keymaps +default-minibuffer-maps
  "C-S-w"    #'cae-yank-word-to-minibuffer)
(after! isearch
  (map! :map isearch-mode-map
        "C-w" nil
        "C-S-w" #'isearch-yank-word-or-char))

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
(setq! evil-ex-substitute-global t
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

(advice-add #'evil-scroll-page-down :around #'cae-evil-scroll-page-down-a)
(advice-add #'evil-scroll-page-up :around #'cae-evil-scroll-page-up-a)

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


(when (modulep! :completion corfu)
  (map! :prefix "C-x"
        (:when (modulep! :cae ai)
         :i "C-c" #'copilot-complete)))

;;(when (modulep! :completion corfu)
;;  (map!
;;   :i "C-n" #'cae-corfu-popup-and-first
;;   :i "C-p" #'cae-corfu-popup-and-last
;;   (:prefix "C-x"
;;    :i "C-c" #'copilot-complete
;;    :i "C-f" #'cape-file
;;    :i "C-l" #'cae-cape-history-or-line
;;    :i "C-s" #'yas-insert-snippet
;;    :i "C-]" #'cae-cape-lsp
;;    :i "C-r" #'cae-cape-keyword-or-dict
;;    :i "s" #'cape-elisp-symbol))
;;  (define-key! :keymaps +default-minibuffer-maps
;;    "C-x C-c" #'copilot-complete
;;    "C-x C-f" #'cape-file
;;    "C-x C-l" #'cae-cape-history-or-line
;;    "C-x C-s" #'yasnippet-capf
;;    "C-x C-]" #'cae-cape-lsp
;;    "C-x C-r" #'cae-cape-keyword-or-dict
;;    "C-x s" #'cape-elisp-symbol)
;;
;;  (after! corfu
;;    ;; I just use `<prior>' and `<next>'. These keybindings conflict with
;;    ;; others.
;;    (map! :map corfu-map
;;          :i "C-u" nil
;;          :i "C-d" nil
;;          :i "C-f" nil
;;          :i "C-b" nil)))

;; TODO Fix this to work with `consult-yasnippet'.
;;(defadvice! cae-evil-insert-state-a (&rest _)
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

;; It'd be better to contribute bindings to `evil-collection' but this is okay.
(evil-set-initial-state #'font-lock-studio-mode 'emacs)

(defun cae-evil-mu4e-enter-insert-mode (&rest _)
  (when (eq evil-state 'normal)
    (call-interactively #'evil-append)))
(advice-add #'compose-mail :after #'cae-evil-mu4e-enter-insert-mode)

(use-package! evil-owl
  :hook (doom-first-input . evil-owl-mode))

(after! evil-snipe
  (cl-pushnew #'calendar-mode evil-snipe-disabled-modes))

;;; Chords

(after! evil-escape
  (remove-hook 'evil-escape-inhibit-functions #'+evil-inhibit-escape-in-minibuffer-fn))
(defadvice! cae-evil-escape-fix-for-restore-point ()
  :after #'evil-escape-pre-command-hook
  (let ((esc-func (evil-escape-func)))
    (when (eq this-command esc-func)
      (setq real-this-command esc-func))))

(use-package! key-chord
  :defer t :init
  (advice-add #'evil-escape-mode :override #'key-chord-mode)
  :config
  (setq key-chord-two-keys-delay 0.2)
  (key-chord-define evil-insert-state-map "jj" #'cae-call-leader-map)
  (key-chord-define evil-insert-state-map "qj" #'cae-ispell-word-then-abbrev)
  (key-chord-define evil-insert-state-map "qk" #'evil-escape)
  (key-chord-define-global "jk" #'evil-escape)
  (key-chord-define-global "qj" #'cae-ispell-word-then-abbrev)
  (key-chord-define-global "qk" #'evil-escape)
  (after! evil-escape
    (key-chord-define evil-insert-state-map "jk" #'evil-escape)))

;;Local Variables:
;;eval: (unless (modulep! :editor evil) (remove-hook 'write-file-functions #'eval-buffer t))
;;End:
