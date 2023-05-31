;;; lisp/cae-bindings.el -*- lexical-binding: t; -*-

;; Doom should not bind leader key prefixes to keys which are not alphanumeric
;; because then they can be overwriting other packages' keybindings. As an
;; example, Org mode has `C-c !' bound to `org-time-stamp-inactive' and `C-c &'
;; bound to `org-mark-ring-goto'.
(when (modulep! :checkers syntax)
  (after! which-key
    (setq which-key-replacement-alist
          (delete '(("\\`C-c !\\'") nil . "checkers")
                  which-key-replacement-alist)))
  (setq flycheck-keymap-prefix (kbd "C-c C"))
  (map! :leader
        (:prefix ("C" . "checkers"))))
(when (modulep! :editor snippets)
  (let ((snippet-prefix "S"))
    (dolist (p (cdr (lookup-key doom-leader-map "&")))
      (cl-destructuring-bind (key . binding) p
        (define-key doom-leader-map (kbd (concat (format "%s " snippet-prefix)
                                                 (char-to-string key))) binding)))
    ;;(after! yasnippet
    ;;  (define-key yas-minor-mode-map (kbd "C-c &") nil))
    (defvar yas-minor-mode-map (make-sparse-keymap))
    (define-key doom-leader-map "&" nil)
    (after! which-key
      (setq which-key-replacement-alist
            (let ((case-fold-search nil))
              (cl-mapcar (lambda (x)
                           (when (car-safe (car x))
                             (setf (car (car x))
                                   (replace-regexp-in-string
                                    "C-c &"
                                    (format "C-c %s" snippet-prefix)
                                    (car-safe (car x)))))
                           x)
                         which-key-replacement-alist))))))

;; Doom binds it's folding prefix to `C-c C-f' which is a keybinding used by
;; many major modes.
(when (and (modulep! :editor fold)
           (not (modulep! :editor evil)))
  (defvar doom-fold-map (lookup-key doom-leader-map "\C-f"))
  (define-key doom-leader-map "\C-f" nil)
  (after! which-key
    (setq which-key-replacement-alist
          (delete '(("\\`C-c C-f\\'") nil . "fold")
                  which-key-replacement-alist)))
  (map! :leader
        (:prefix ("F" . "fold")
         ;;"k"     #'vimish-fold-delete
         ;;"K" #'vimish-fold-delete-all
         "t"     #'+fold/toggle
         "C" #'+fold/close-all
         "o"     #'+fold/open
         "O" #'+fold/open-all)))

;; I don't use Deft.
(when (and (not (modulep! :ui deft))
           (eq (lookup-key doom-leader-map "nd")
               'deft))
  (define-key doom-leader-map "nd" nil))

;; Add some descriptions for built-in prefixes.
(map! :prefix ("M-s h"   . "highlight")
      :prefix ("C-x 4"   . "other-window")
      :prefix ("C-x 5"   . "other-frame")
      :prefix ("C-x 8"   . "unicode")
      :prefix ("C-x a"   . "abbrev")
      :prefix ("C-x a i" . "inverse")
      :prefix ("C-x n"   . "narrow")
      :prefix ("C-x r"   . "register")
      :prefix ("C-x t"   . "tab-bar")
      :prefix ("C-x t ^" . "detach")
      :prefix ("C-x v"   . "version-control")
      :prefix ("C-x w"   . "window")
      :prefix ("C-x w ^" . "detach")
      :prefix ("C-x x"   . "extra")
      :prefix ("C-x C-a" . "gud")
      :prefix ("C-x X"   . "edebug")
      :prefix ("C-x C-k" . "kmacro")
      :prefix ("C-x RET" . "MULE"))

;; I like to add bind `<leader> h' to `help-map' like how Doom Emacs does for
;; Evil.
(map! :leader :desc "help" "h" help-map)

;; Remove redundant `consult-history' keybinding.
(define-key!
  :keymaps (append +default-minibuffer-maps
                   (when (modulep! :editor evil +everywhere)
                     '(evil-ex-completion-map)))
  "C-s" nil)                          ;We already have `consult-history' bound
                                        ;to `M-r' and `M-s'. This way we can use
                                        ;`C-s' to search in the minibuffer.

;; Only use `embark-act-key' for `embark-act'. Remove all other bindings.
(let ((embark-act-key "<f8>"))
  (when (eq (lookup-key doom-leader-map "a")
            'embark-act)
    (define-key doom-leader-map "a" nil))
  (map! embark-act-key #'embark-act
        (:when (modulep! :completion vertico)
         "C-;" nil
         (:map minibuffer-local-map
          "C-;" nil
          embark-act-key #'embark-act)))
  (eval
   `(after! embark
      (setq embark-cycle-key ,embark-act-key))
   t))

;; General keybindings.
(map! [remap backward-kill-word] #'doom/delete-backward-word ;Do not litter the kill-ring.
      [remap upcase-word] #'upcase-dwim
      [remap downcase-word] #'downcase-dwim
      [remap capitalize-word] #'capitalize-dwim
      [remap ispell-word] #'cae-ispell-word-then-abbrev
      "C-x 4 I" #'ibuffer-other-window
      [remap ibuffer] #'ibuffer-jump    ;This way
                                        ;I can do `C-x C-b =' to quickly diff a
                                        ;buffer with its file.
      "C-x _" #'shrink-window           ;Dual to `C-x ^'.
      "C-x O" #'other-window-previous   ;Dual to `C-x o'.
      "C-x !" #'doom/window-enlargen
      "C-x M-o" #'ace-swap-window
      "C-x x o" #'ov-clear
      "M-Z" #'zap-up-to-char
      [C-i] #'doom/dumb-indent
      "C-S-i" #'doom/dumb-dedent
      [remap doom/backward-to-bol-or-indent] #'beginning-of-line
      [remap doom/sudo-this-file] #'cae-toggle-sudo
      [remap er/expand-region] #'eri/expand-region
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
(map! [remap delete-char] #'cae-delete-char
      ")" #'cae-insert-closing-paren)

;; This one is because I bind `C-h' to a dedicated key on my keyboard.
(unless (lookup-key help-map (kbd "SPC"))
  (define-key help-map (kbd "SPC") #'cae-pop-mark))

(map! :prefix "C-z"
      "n" #'avy-goto-line-below
      "p" #'avy-goto-line-above
      ;;"y" #'avy-copy-region
      "c" #'avy-goto-char
      ;;"m" #'avy-move-region
      "l" #'avy-goto-line
      "e" #'avy-goto-end-of-line
      "." #'cae-avy-symbol-at-point
      ;;"k" #'avy-kill-region
      ;;"w" #'avy-kill-ring-save-region
      "a" #'cae-avy-embark-act-on-region
      "j" #'avy-goto-word-1
      "o" #'switch-window
      "0" #'switch-window-then-delete
      "1" #'switch-window-then-maximize
      "2" #'switch-window-then-split-horizontally
      "3" #'switch-window-then-split-vertically
      "4" #'switch-window-then-kill-buffer
      ;;"r" #'avy-resume ; `avy-resume' is too buggy to be useful.
      "SPC" #'avy-goto-char-timer
      (:map isearch-mode-map
       "j" #'avy-isearch))

(when (modulep! :completion vertico)
  (after! vertico
    (map! :map vertico-map
          "M-j" #'vertico-quick-jump
          "M-i" #'vertico-quick-exit)))

(after! embark
  (map! :map embark-collect-mode-map
        "M-j" #'avy-embark-collect-choose
        "M-i" #'avy-embark-collect-act))
(when (modulep! :private corfu)
  (after! corfu
    (map! :map corfu-map
          "M-j" #'corfu-quick-jump
          "M-i" #'corfu-quick-insert)))

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
(map! :leader
      :desc "Copy link" "sy" #'link-hint-copy-link)

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

;; This minor mode is defined so that there keybindings can be temporarily
;; turned off for multiple cursors and similar modes where completion is not a
;; good idea.
(define-minor-mode cae-completion-mode
  "A minor mode for convenient completion keybindings."
  :global t
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-. c")   #'completion-at-point)
            (define-key map (kbd "C-. t")   #'complete-tag)
            (define-key map (kbd "C-. d")   #'cape-dabbrev)
            (define-key map (kbd "C-. f")   #'cape-file)
            (define-key map (kbd "C-. k")   #'cape-keyword)
            (define-key map (kbd "C-. h")   #'cape-history)
            (define-key map (kbd "C-. e")   #'cape-symbol)
            (define-key map (kbd "C-. a")   #'cape-abbrev)
            (define-key map (kbd "C-. i")   #'cape-ispell)
            (define-key map (kbd "C-. l")   #'cape-line)
            (define-key map (kbd "C-. w")   #'cape-dict)
            (define-key map (kbd "C-. \\")  #'cape-tex)
            (define-key map (kbd "C-. _")   #'cape-tex)
            (define-key map (kbd "C-. ^")   #'cape-tex)
            (define-key map (kbd "C-. &")   #'cape-sgml)
            (define-key map (kbd "C-. r")   #'cape-rfc1345)
            (define-key map (kbd "C-. .")   #'copilot-complete)
            (when (modulep! :editor multiple-cursors)
              (define-key map (kbd "C-. C-.")  #'mc/unfreeze-fake-cursors))
            map))
(cae-completion-mode +1)

(after! cc-mode
    (if (cae-display-graphic-p)
        (map! :map c-mode-base-map "<tab>" #'indent-for-tab-command)
      (map! :map c-mode-base-map "TAB" #'indent-for-tab-command)))

  (when (modulep! :editor snippets)
    (map! (:when (modulep! :completion vertico)
           [remap yas-insert-snippet] #'consult-yasnippet)
          :map yas-minor-mode-map
          "C-c & C-s" nil
          "C-c & C-n" nil
          "C-c & C-v" nil))

(when (modulep! :completion vertico)
  (map! (:map help-map
         "C-m" #'describe-keymap
         "<return>" #'info-emacs-manual)
        ;; C-x bindings (ctl-x-map)
        "C-x M-:" #'consult-complex-command ;orig. repeat-complex-command
        ;; Custom M-# bindings for fast register access
        "M-#" #'consult-register-load
        "M-'" #'consult-register-store  ;orig. abbrev-prefix-mark (unrelated)
        "C-M-#" #'consult-register
        [remap jump-to-register] #'consult-register-load
        ;; Other custom bindings
        ;; M-g bindings (goto-map)
        "M-g e" #'consult-compile-error
        "M-g g" #'consult-goto-line     ;orig. goto-line
        "M-g M-g" #'consult-goto-line   ;orig. goto-line
        "M-g o" #'consult-outline       ;Alternative: consult-org-heading
        "M-g m" #'consult-mark
        "M-g k" #'consult-global-mark
        "M-g I" #'consult-imenu-multi
        ;; M-s bindings (search-map)
        "M-s k" #'consult-keep-lines
        "M-s u" #'consult-focus-lines
        ;; Isearch integration
        "M-s e" #'consult-isearch-history
        :map isearch-mode-map
        "M-e" #'consult-isearch-history   ;orig. isearch-edit-string
        "M-s e" #'consult-isearch-history ;orig. isearch-edit-string
        ;; Minibuffer history
        :map minibuffer-local-map
        ;; "M-s" #'consult-history     ;orig. next-matching-history-element
        "M-r" #'consult-history         ;orig. previous-matching-history-element
        ;; Redundant with Doom's :config default bindings
        :map global-map
        "M-g f" #'consult-flymake
        (:when (and (modulep! :checkers syntax)
                    (not (modulep! :checkers syntax +flymake)))
         "M-g f" #'consult-flycheck)
        (:unless (modulep! :config default)
         "M-s d" #'consult-find     ;does not cache files like Doom & Projectile
                                        ;also slower than `fd'. See Minad's comment
                                        ;in
                                        ;https://github.com/minad/consult/issues/363
         "M-s r" #'consult-ripgrep
         "M-s D" #'consult-locate)
        [remap Info-search] #'consult-info
        :map help-map
        "TAB" #'consult-info
        "W" #'consult-man
        :leader
        "M-x" #'consult-mode-command
        (:desc "Keyboard macro"  "ik" #'consult-kmacro))
  (when (modulep! :tools debugger +lsp)
    (after! dap-ui
      (map! :map dap-ui-repl-mode-map
            "M-r" #'consult-history))))
