;;; lisp/cae-evil.el -*- lexical-binding: t; -*-

;; I use a split keyboard and map backspace to my left thumb key.
;;(lookup-key evil-normal-state-map doom-localleader-key)
(after! evil-easymotion
  (map! :map evilem-map
        "DEL" (lookup-key evilem-map "\s")))

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

;; Define help better keybinding help commands for Evil.
(map! :n "C-S-h" #'cae-embark-bindings-for-current-state
      :n "C-M-?" #'cae-which-key-show-current-state-bindings)
(after! help
  (map! :map help-map "bn" #'cae-show-normal-state-bindings))

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
(after! info
  (map! :map Info-mode-map
        :m "/" #'isearch-forward-regexp
        :m "?" #'isearch-backward-regexp))

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

(define-key! :keymaps +default-minibuffer-maps
  "C-w"    #'cae-yank-word-to-minibuffer)

(after! evil-easymotion
  (map! :map evilem-map
        "RET" #'cae-avy-embark-act-on-region
        "TAB" #'avy-goto-word-1))

(map! :prefix "g"
      :m "[" #'backward-page
      :m "]" #'cae-forward-page)

(after! evil-snipe
  (setq evil-snipe-scope 'line))
(setq evil-ex-substitute-global t
      evil-move-cursor-back nil
      evil-vsplit-window-right t
      evil-kill-on-visual-paste nil
      evil-split-window-below t
      evil-v$-excludes-newline t)

(use-package! evil-owl
  :hook (doom-first-input . evil-owl-mode))

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
          :n "] DEL" #'+evil/insert-newline-below
          :n "[ DEL" #'+evil/insert-newline-above
          :m "[6" #'cae-unimpaired-b64-encode
          :m "]6" #'cae-unimpaired-b64-decode)))

(when (modulep! :completion corfu)
  (map! (:prefix "C-x"
         :i "C-c" #'copilot-complete
         :i "C-f" #'cape-file
         :i "C-s" #'yasnippet-capf
         :i "C-l" #'cape-line
         (:after cape
          :i "C-d" (cape-interactive-capf (cape-capf-super #'cape-dabbrev #'cape-dict #'cape-keyword)))
         :i "s" #'cape-elisp-symbol
         :i "C-n" nil
         :i "C-p" nil)))

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

(defun cae-evil-mu4e-enter-insert-mode ()
  (when (eq evil-state 'normal)
    (call-interactively #'evil-append)))
(add-hook 'mu4e-compose-mode-hook #'cae-evil-mu4e-enter-insert-mode 90)

(use-package! evil-textobj-synax
  :defer t :init
  (after! evil
    (map! :map evil-inner-text-objects-map
          "h" #'evil-i-syntax
          :map evil-outer-text-objects-map
          "h" #'evil-a-syntax)))

;;Local Variables:
;;eval: (unless (modulep! :editor evil) (remove-hook 'write-file-functions #'eval-buffer t))
;;End:
