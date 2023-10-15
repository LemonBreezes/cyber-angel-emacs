  ;;; lisp/cae-bindings.el -*- lexical-binding: t; -*-

;; This is so that I don't get an error if my `cae-doom-emacs.patch' is not
;; applied.
(unless (symbol-function 'doom-leader-map)
  (fset 'doom-leader-map doom-leader-map))

(map! :map general-override-mode-map
      :desc "<leader>" :nmv "DEL" #'doom-leader-map
      :desc "<leader>" :nmv "<backspace>" #'doom-leader-map)
(define-key key-translation-map (kbd "SPC DEL") (kbd "SPC SPC"))
(define-key key-translation-map (kbd "<backspace> DEL") (kbd "<backspace> SPC"))

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
          embark-act-all-alt-key #'embark-act-all)
         (:map isearch-mode-map
          [remap isearch-describe-bindings] (cmd! () (embark-bindings-in-keymap isearch-mode-map)
                                                  (when isearch-mode (isearch-update)))))
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
      [remap kill-current-buffer] #'cae-kill-current-buffer
      "C-x 4 I" #'ibuffer-other-window
      "C-x 4 -" #'dired-jump-other-window
      [remap ibuffer] #'ibuffer-jump    ;This way
                                        ;I can do `C-x C-b =' to quickly diff a
                                        ;buffer with its file.
      "C-x _" #'shrink-window           ;Dual to `C-x ^'.
      "C-x O" #'other-window-previous   ;Dual to `C-x o'.
      "C-x !" #'doom/window-enlargen
      "C-x x c" #'cae-edit-indirect-dwim
      "C-x M-o" #'ace-swap-window
      "C-x M-t" #'transpose-frame
      ;;"C-x x N" #'cae-make-new-buffer ;TODO Check if this is necessary next
                                        ;time I disable Evil.
      "C-x x o" #'ov-clear
      "C-x P" #'pop-to-buffer
      "M-Z" #'zap-up-to-char
      "M-R" #'cae-sp-raise-sexp
      [remap dabbrev-expand] #'hippie-expand
      (:when (modulep! :completion corfu)
       [remap dabbrev-completion] #'cape-dabbrev)
      [C-i] #'doom/dumb-indent
      "C-S-i" #'doom/dumb-dedent
      "C-S-h" #'embark-bindings
      "<escape>" #'keyboard-quit
      "<f6>" #'embrace-commander
      "<f7>" #'eri/expand-region
      [remap doom/sudo-this-file] #'cae-toggle-sudo
      (:map process-menu-mode-map
       "o" #'link-hint-open-link)
      (:when (modulep! :completion vertico)
       [remap apropos] nil)             ;`consult-apropos' is obsolete.
      (:after vertico
       (:map vertico-map
        "<prior>" #'vertico-scroll-down
        "<next>" #'vertico-scroll-up
        "C-z" #'cae-embark-act-with-completing-read)))
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

(when (modulep! :completion corfu)
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
;; This is so that `describe-key-briefly' is more convenient to use with the
;; `<f1>' prefix.
(define-key help-map (kbd "<f1>") #'describe-key-briefly)

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

(after! cc-mode
  (if (cae-display-graphic-p)
      (map! :map c-mode-base-map "<tab>" #'indent-for-tab-command)
    (map! :map c-mode-base-map "TAB" #'indent-for-tab-command)))


;;; Fixup leader key

;; I don't use Deft.
(when (and (not (modulep! :ui deft))
           (eq (lookup-key doom-leader-map "nd")
               'deft))
  (define-key doom-leader-map "nd" nil))

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
  (dolist (p '(("4" . "other-window")
               ("5" . "other-frame")
               ("8" . "unicode")
               ("a" . "abbrev")
               ("a i" . "inverse")
               ("n" . "narrow")
               ("t" . "tab-bar")
               ("t ^" . "detach")
               ("v" . "version-control")
               ("v b" . "branch")
               ("v M" . "mergebase")
               ("w" . "window")
               ("w ^" . "detach")
               ("x" . "extra")
               ("C-k" . "kmacro")
               ("RET" . "MULE")))
    (which-key-add-keymap-based-replacements ctl-x-map (car p) (cdr p)))
  (after! edebug
    (which-key-add-keymap-based-replacements ctl-x-map "X" "edebug")
    (which-key-add-keymap-based-replacements emacs-lisp-mode-map
      "C-x C-a" "edebug"))
  (after! gud
    (fset 'gud-global-map gud-global-map)
    (map! :map ctl-x-map "C-a" #'gud-global-map))
  (after! register
    (which-key-add-keymap-based-replacements ctl-x-map "r" "register")))
(fset 'ctl-x-map ctl-x-map)



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
   "M-X" #'consult-mode-command)
  (map! :map help-map "TAB" #'consult-info)
  (when (modulep! :tools debugger +lsp)
    (after! dap-ui
      (map! :map dap-ui-repl-mode-map
            "M-r" #'consult-history))))
(map! :leader
      (:when (modulep! :completion vertico)
       (:prefix "i"
        :desc "Keyboard macro" "k" #'consult-kmacro)
       (:prefix "s"
        :desc "Copy link" "y" #'link-hint-copy-link
        :desc "Jump to section" "h" #'consult-outline))
      (:when (modulep! :tools editorconfig)
       (:prefix "c"
        :desc "Format whitespace" "C-f" #'editorconfig-format-buffer))
      (:prefix "t"
       :desc "Column indicator" "C" #'vline-mode
       :desc "Font lock mode" "C-f" #'font-lock-mode))

(unless (modulep! :editor evil)
  (after! org
    (map! :map org-mode-map
          "M-RET" #'org-insert-heading
          "M-RET" #'cae-evil-org-insert-heading
          "M-S-RET" #'cae-evil-org-insert-todo-heading
          "M-<return>" #'org-insert-heading
          "M-S-<return>" #'org-insert-todo-heading)))

(after! treemacs
  (when (modulep! :completion vertico)
    (map! :map treemacs-mode-map
          "j" #'consult-line)
    (when (modulep! :editor evil)
      (map! :map evil-treemacs-state-map
            "J" #'consult-line))))
