  ;;; lisp/cae-bindings.el -*- lexical-binding: t; -*-

;; Allow escape to exit the minibuffer.
(define-key! :keymaps +default-minibuffer-maps
  [escape] #'abort-recursive-edit)

;; Remove redundant `consult-history' keybinding.
(define-key!
  :keymaps (append +default-minibuffer-maps
                   (when (modulep! :editor evil +everywhere)
                     '(evil-ex-completion-map)))
  "C-s" nil)                          ;We already have `consult-history' bound
                                        ;to `M-r' and `M-s'. This way we can use
                                        ;`C-s' to search in the minibuffer.

;; Only use `embark-act-key' for `embark-act'. Remove all other bindings.
(let ((embark-act-key "C-;")
      (embark-act-alt-key "<f8>")
      (embark-act-all-key "C-:")
      (embark-act-all-alt-key "<f9>"))
  (when (eq (lookup-key doom-leader-map "a")
            'embark-act)
    (define-key doom-leader-map "a" nil)
    (after! which-key
      (setq which-key-replacement-alist
            (cl-remove-if (lambda (x) (equal (cddr x) "Actions"))
                          which-key-replacement-alist))))
  (map! (:when (modulep! :completion vertico)
         "C-;" nil
         (:map minibuffer-local-map
          "C-;" nil
          embark-act-key #'cae-embark-act
          embark-act-alt-key #'cae-embark-act
          embark-act-all-key #'embark-act-all
          embark-act-all-alt-key #'embark-act-all))
        embark-act-key #'cae-embark-act
        embark-act-alt-key #'cae-embark-act
        embark-act-all-key #'embark-act-all
        embark-act-all-alt-key #'embark-act-all)
  (eval
   `(after! embark
      (setq embark-cycle-key ,embark-act-key))
   t))

(after! embark
  ;; `elp' instrument package commands from `embark-package-map' are not mapped
  ;; in `+vertico/embark-doom-package-map'.
  (map! :map +vertico/embark-doom-package-map
        "t" #'try)
  (map! :map embark-region-map
        "k" #'cae-kill-region))

;; General keybindings.
(map! [remap backward-kill-word] #'doom/delete-backward-word ;Do not litter the kill-ring.
      [remap upcase-word] #'upcase-dwim
      [remap downcase-word] #'downcase-dwim
      [remap capitalize-word] #'capitalize-dwim
      [remap ispell-word] #'cae-ispell-word-then-abbrev
      [remap exchange-point-and-mark] #'cae-exchange-point-and-mark
      "C-x 4 I" #'ibuffer-other-window
      [remap ibuffer] #'ibuffer-jump    ;This way
                                        ;I can do `C-x C-b =' to quickly diff a
                                        ;buffer with its file.
      "C-x _" #'shrink-window           ;Dual to `C-x ^'.
      "C-x O" #'other-window-previous   ;Dual to `C-x o'.
      "C-x !" #'doom/window-enlargen
      "C-x M-o" #'ace-swap-window
      "C-x M-t" #'transpose-frame
      "C-x x N" #'cae-make-new-buffer
      "C-x x o" #'ov-clear
      "C-x P" #'pop-to-buffer
      "M-Z" #'zap-up-to-char
      "M-/" #'cae-dabbrev-expand
      [C-i] #'doom/dumb-indent
      "C-S-i" #'doom/dumb-dedent
      "<escape>" #'keyboard-quit
      [remap doom/backward-to-bol-or-indent] #'beginning-of-line
      [remap doom/sudo-this-file] #'cae-toggle-sudo
      [remap er/expand-region] #'eri/expand-region
      "<f7>" #'er/expand-region
      (:after transient
       (:map transient-map
        "<f6>" #'transient-quit-all))
      (:map process-menu-mode-map
       "o" #'link-hint-open-link)
      (:when (modulep! :tools lookup)
       [remap xref-find-definitions] #'cae-lookup-definition-dwim)
      (:when (modulep! :completion vertico)
       [remap apropos] nil)             ;`consult-apropos' is obsolete.
      (:after man
       :map Man-mode-map
       "o" #'ace-link-man)
      (:after vertico
       (:map vertico-map
        "<prior>" #'vertico-scroll-down
        "<next>" #'vertico-scroll-up
        "C-z" #'cae-embark-act-with-completing-read
        "<f6>" #'cae-vertico-cheatsheet-hydra/body))
      (:after eww
       :map eww-mode-map
       "o" #'ace-link-eww))
(define-key resize-window-repeat-map "_" #'shrink-window)

;; Allow deleting a closing paren if parens are unbalanced. Also allow inserting
;; a closing paren if parens are unbalanced.
(map! [remap delete-char] #'cae-delete-char
      ")" #'cae-insert-closing-paren)

;; Use `TAB' instead of `RET' for outline cycling buttons
(after! outline
  (map! :map outline-overlay-button-map
        "RET" nil
        "TAB" #'outline-cycle))

(map! :leader
      :prefix "t"
      :desc "Column indicator" "C" #'vline-mode)

(when (modulep! :private corfu)
  (after! corfu
    (map! "C-SPC" (lookup-key global-map (kbd "C-@"))
          :map corfu-map
          "C-M-i" #'corfu-move-to-minibuffer
          "<escape>" #'keyboard-quit
          ;; I use `TAB' instead. I don't like how the `RET' keybinding prevents
          ;; me from exiting the minibuffer while the completion menu is open.
          "RET" nil)))

;; This one is because I bind `C-h' to a dedicated key on my keyboard.
(define-key help-map (kbd "SPC") #'cae-pop-mark)

(after! diff-mode
  (map! :map diff-mode-map
        "q" #'kill-this-buffer))

;; Monkey fix `project.el' overriding the `C-x p' keybinding.
(when (modulep! :ui popup)
  (unless (boundp 'cae-fix-popup-other-keybinding-idle-timer)
    (defvar cae-fix-popup-other-keybinding-idle-timer nil)
    (setq cae-fix-popup-other-keybinding-idle-timer
          (run-with-idle-timer
           3 t
           (cae-defun cae-fix-popup-other-keybinding ()
             (define-key ctl-x-map "p" nil)
             (map! :map ctl-x-map
                   "p" #'+popup/other))))))

;; I'm surprised Doom Emacs doesn't bind a key for copying links.
(map! :leader :desc "Copy link" "sy" #'link-hint-copy-link)

(advice-add #'persp-set-keymap-prefix :override #'ignore)

;; Bind `tab-bar' commands consistently with the built-in keybindings.
(defadvice! cae-tab-bar-define-keys-a ()
  :after #'tab-bar--define-keys
  (unless (global-key-binding [(control f4)])
    (global-set-key [(control f4)] #'tab-close)))
(defadvice! cae-tab-bar-undefine-keys-a ()
  :after #'tab-bar--undefine-keys
  (when (eq (global-key-binding [(control f4)]) #'tab-close)
    (global-unset-key [(control f4)])))

;; Do not query before deleting a frame, since we can undo frame deletions.
(global-set-key [remap delete-frame] nil)
(global-set-key [remap delete-other-windows] #'doom/window-maximize-buffer)


;;; Fixup leader key

;; Doom should not bind leader key prefixes to keys which are not alphanumeric
;; because then they can be overwriting other packages' keybindings. As an
;; example, Org mode has `C-c !' bound to `org-time-stamp-inactive' and `C-c &'
;; bound to `org-mark-ring-goto'.
(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake))
           (not (modulep! :editor evil)))
  (setq flycheck-keymap-prefix (kbd "C-c C"))
  (map! :leader (:prefix-map ("C" . "checkers"))))

;; Doom binds it's folding prefix to `C-c C-f' which is a keybinding used by
;; many major modes.
(when (and (modulep! :editor fold)
           (not (modulep! :editor evil)))
  (defvar doom-fold-map (lookup-key doom-leader-map "\C-f"))
  (define-key doom-leader-map "\C-f" nil)
  (unless (modulep! :editor evil)
    (map! :leader
          (:prefix-map ("F" . "fold")
           "k" #'vimish-fold-delete
           "K" #'vimish-fold-delete-all
           "c" #'vimish-fold
           "t" #'+fold/toggle
           "C" #'+fold/close-all
           "o" #'+fold/open
           "O" #'+fold/open-all))))

;; I don't use Deft.
(when (and (not (modulep! :ui deft))
           (eq (lookup-key doom-leader-map "nd")
               'deft))
  (define-key doom-leader-map "nd" nil))

;; I like to add bind `<leader> h' to `help-map' like how Doom Emacs does for
;; Evil.
(unless (modulep! :editor evil)
  (map! :leader :desc "help" "h" help-map))

(when (modulep! :editor snippets)
  (map! (:when (modulep! :completion vertico)
         [remap yas-insert-snippet] #'consult-yasnippet)
        :map yas-minor-mode-map
        "C-c & C-s" nil
        "C-c & C-n" nil
        "C-c & C-v" nil))


;;; Extra which-key descriptions

;; Add some descriptions for built-in prefixes.
(after! which-key
  (which-key-add-keymap-based-replacements search-map "h" "highlight")
  (which-key-add-keymap-based-replacements help-map "4" "other-window")
  (which-key-add-key-based-replacements "C-x r" "register")
  (which-key-add-key-based-replacements "C-x w" "window")
  (which-key-add-key-based-replacements "C-x X" "debug")
  (dolist (p '(("4" . "other-window")
               ("5" . "other-frame")
               ("8" . "unicode")
               ("a" . "abbrev")
               ("a i" . "inverse")
               ("n" . "narrow")
               ;;("r" . "register")
               ("t" . "tab-bar")
               ("t ^" . "detach")
               ("v" . "version-control")
               ("v b" . "branch")
               ("v M" . "mergebase")
               ;;("w" . "window")
               ("w ^" . "detach")
               ("x" . "extra")
               ("C-a" . "gud")
               ("X" . "edebug")
               ("C-k" . "kmacro")
               ("RET" . "MULE")
               ;;("X" . "debug")
               ))
    (which-key-add-keymap-based-replacements ctl-x-map (car p) (cdr p))))
(define-prefix-command 'ctl-x-r-map)


;;; Avy keybindings

;; (unless (modulep! :editor evil)
;;   (map! :prefix "C-z"
;;         "n" #'avy-goto-line-below
;;         "p" #'avy-goto-line-above
;;         "t" #'tabgo
;;         ;;"y" #'avy-copy-region
;;         "c" #'avy-goto-char
;;         ;;"m" #'avy-move-region
;;         "l" #'avy-goto-line
;;         "e" #'avy-goto-end-of-line
;;         "." #'cae-avy-symbol-at-point
;;         ;;"k" #'avy-kill-region
;;         ;;"w" #'avy-kill-ring-save-region
;;         "a" #'cae-avy-embark-act-on-region
;;         "j" #'avy-goto-word-1
;;         (:when (modulep! :editor fold)
;;          "f" #'vimish-fold-avy)
;;         "o" #'switch-window
;;         "0" #'switch-window-then-delete
;;         "1" #'switch-window-then-maximize
;;         "2" #'switch-window-then-split-horizontally
;;         "3" #'switch-window-then-split-vertically
;;         "4" #'switch-window-then-kill-buffer
;;         ;;"r" #'avy-resume ; `avy-resume' is too buggy to be useful.
;;         "SPC" #'avy-goto-char-timer
;;         (:map isearch-mode-map
;;          "j" #'avy-isearch))
;;   (when (modulep! :completion vertico)
;;     (after! vertico
;;       (map! :map vertico-map
;;             "M-j" #'vertico-quick-jump
;;             "M-i" #'vertico-quick-exit)))
;;   (after! embark
;;     (map! :map embark-collect-mode-map
;;           "M-j" #'avy-embark-collect-choose
;;           "M-i" #'avy-embark-collect-act))
;;   (when (modulep! :private corfu)
;;     (after! corfu
;;       (map! :map corfu-map
;;             "M-j" #'corfu-quick-jump
;;             "M-i" #'corfu-quick-insert))))


;;; Completion keybindings

;; This minor mode is defined so that there keybindings can be temporarily
;; turned off for multiple cursors and similar modes where completion is not a
;; good idea.
(define-prefix-command 'cae-completion-prefix-map)
(define-key cae-completion-prefix-map "c" #'completion-at-point)
(define-key cae-completion-prefix-map "t" #'complete-tag)
(define-key cae-completion-prefix-map "d" #'cape-dabbrev)
(define-key cae-completion-prefix-map "f" #'cape-file)
(define-key cae-completion-prefix-map "k" #'cape-keyword)
(define-key cae-completion-prefix-map "h" #'cape-history)
(define-key cae-completion-prefix-map "e" #'cape-symbol)
(define-key cae-completion-prefix-map "a" #'cape-abbrev)
(define-key cae-completion-prefix-map "l" #'cape-line)
(define-key cae-completion-prefix-map "w" #'cape-dict)
(define-key cae-completion-prefix-map "\\" #'cape-tex)
(define-key cae-completion-prefix-map "_" #'cape-tex)
(define-key cae-completion-prefix-map "^" #'cape-tex)
(define-key cae-completion-prefix-map "&" #'cape-sgml)
(define-key cae-completion-prefix-map "r" #'cape-rfc1345)
(define-key cae-completion-prefix-map "." #'copilot-complete)
(when (modulep! :editor multiple-cursors)
  (define-key cae-completion-prefix-map (kbd "C-.") #'mc/unfreeze-fake-cursors)
  (define-key cae-completion-prefix-map (kbd "<f5>") #'mc/unfreeze-fake-cursors))
(define-minor-mode cae-completion-mode
  "A minor mode for convenient completion keybindings."
  :global t
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-.") #'cae-completion-prefix-map)
            (define-key map (kbd "<f5>") #'cae-completion-prefix-map)
            map)
  :group 'cae)
(cae-completion-mode +1)

(after! cc-mode
  (if (cae-display-graphic-p)
      (map! :map c-mode-base-map "<tab>" #'indent-for-tab-command)
    (map! :map c-mode-base-map "TAB" #'indent-for-tab-command)))


;;; Consult keybindings

(when (modulep! :completion vertico)
  (map!                                ;; C-x bindings (ctl-x-map)
   "C-x M-:" #'consult-complex-command ;orig. repeat-complex-command
   ;; Custom M-# bindings for fast register access
   "M-#" #'consult-register-load
   "M-'" #'consult-register-store       ;orig. abbrev-prefix-mark (unrelated)
   "C-M-#" #'consult-register
   [remap jump-to-register] #'consult-register-load
   ;; Other custom bindings
   ;; M-g bindings (goto-map)
   "M-g e" #'consult-compile-error
   "M-g g" #'consult-goto-line          ;orig. goto-line
   "M-g M-g" #'consult-goto-line        ;orig. goto-line
   "M-g o" #'consult-outline            ;Alternative: consult-org-heading
   "M-g m" #'consult-mark
   "M-g k" #'consult-global-mark
   "M-g I" #'consult-imenu-multi
;; M-s bindings (search-map)
   "M-s k" #'consult-keep-lines
   "M-s u" #'consult-focus-lines
   ;; Isearch integration

   :map isearch-mode-map
   "M-e" #'consult-isearch-history      ;orig. isearch-edit-string
   "M-s e" #'consult-isearch-history    ;orig. isearch-edit-string
   ;; Minibuffer history
   :map minibuffer-local-map
   ;; "M-s" #'consult-history     ;orig. next-matching-history-element
   "M-r" #'consult-history     ;orig. previous-matching-history-element
   ;; Redundant with Doom's :config default bindings
   :map global-map
   "M-g f" #'consult-flymake
   (:when (and (modulep! :checkers syntax)
               (not (modulep! :checkers syntax +flymake)))
    "M-g f" #'consult-flycheck)
   (:unless (modulep! :config default)
    "M-s d" #'consult-find          ;does not cache files like Doom & Projectile
                                        ;also slower than `fd'. See Minad's comment
                                        ;in
                                        ;https://github.com/minad/consult/issues/363
    "M-s r" #'consult-ripgrep
    "M-s D" #'consult-locate)
   [remap Info-search] #'consult-info
   "M-X" #'consult-mode-command
   "W" #'consult-man)
  (map! :map help-map "TAB" #'consult-info)
  (map! :leader
        :desc "Keyboard macro"  "ik" #'consult-kmacro)
  (when (modulep! :tools debugger +lsp)
    (after! dap-ui
      (map! :map dap-ui-repl-mode-map
            "M-r" #'consult-history))))
