;;; lisp/cae-bindings.el -*- lexical-binding: t; -*-

;; This is for keymap-based which-key descriptions.
(unless (symbol-function 'doom-leader-map)
  (fset 'doom-leader-map doom-leader-map))

(after! general
  ;; I use a split ergo keyboard with SPC and DEL on the thumb keys.
  (map! :map general-override-mode-map
        :desc "<leader>" :nmv "DEL" #'doom-leader-map
        :desc "<leader>" :nmv "<backspace>" #'doom-leader-map)
  ;; Define the leader key in Emacs state so that I can use it from the minibuffer.
  (map! :map general-override-mode-map
        :desc "<leader>" :g doom-leader-alt-key #'doom-leader-map))
(define-key key-translation-map (kbd "SPC DEL") (kbd "SPC SPC"))
(define-key key-translation-map (kbd "<backspace> DEL") (kbd "<backspace> SPC"))

;; Remove redundant `consult-history' keybinding.
(define-key!
  :keymaps (append +default-minibuffer-maps
                   (when (modulep! :editor evil +everywhere)
                     '(evil-ex-completion-map)))
  "C-s" nil)                         ;We already have `consult-history' bound
                                        ;to `M-r' and `M-s'. This way we can use
                                        ;`C-s' to search in the minibuffer.

;; Only use `embark-act-key' for `embark-act'. Remove all other bindings.
(when (modulep! :completion vertico)
  (when (eq (lookup-key doom-leader-map "a")
            'embark-act)
    (define-key doom-leader-map "a" nil)
    (after! which-key
      (setq which-key-replacement-alist
            (cl-remove-if (lambda (x) (equal (cddr x) "Actions"))
                          which-key-replacement-alist))))
  (map! :map isearch-mode-map
        [remap isearch-describe-bindings]
        (cmd! () (embark-bindings-in-keymap isearch-mode-map)
              (when isearch-mode (isearch-update))))
  (let ((embark-act-keys '("C-;" "<f8>"))
        (embark-act-all-keys '("C-:" "<f9>"))
        (embark-export-keys '("C-c C-;" "C-c ;"))
        (embark-leader-key "e"))
    (eval `(map! :leader :desc "Embark Act" ,embark-leader-key
                 (cmd! ()
                       (require 'embark)
                       (let ((embark-cycle-key ,embark-leader-key))
                         (call-interactively #'embark-act))))
          t)
    (map! "C-;" nil
          (:map minibuffer-local-map
           "C-;" nil))
    (dolist (key embark-act-keys)
      (map! key #'embark-act
            (:map minibuffer-local-map
             key #'embark-act)))
    (dolist (key embark-act-all-keys)
      (map! key #'embark-act-all
            (:map minibuffer-local-map
             key #'embark-act-all)))
    (dolist (key embark-export-keys)
      (map! :map minibuffer-local-map
            key #'embark-export))
    (eval
     `(after! embark
        (setq embark-cycle-key ,(car embark-act-keys)))
     t))

  (after! embark
    ;; `elp' instrument package commands from `embark-package-map' are not mapped
    ;; in `+vertico/embark-doom-package-map'.
    (map! :map +vertico/embark-doom-package-map
          "t" #'try)
    (map! :map embark-region-map
          "k" #'cae-kill-region)))

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
      "C-x j" #'cae-jump-to-random-line
      ;;"C-x x N" #'cae-make-new-buffer ;TODO Check if this is necessary next
                                        ;time I disable Evil.
      "C-x x o" #'ov-clear
      "C-x x p" #'posframe-delete-all
      "C-x P" #'pop-to-buffer
      "M-Z" #'zap-up-to-char
      "M-R" #'cae-sp-raise-sexp
      "M-/" #'hippie-expand
      (:when (modulep! :completion corfu)
       "C-M-/" #'cape-dabbrev
       "C-S-s" #'cae-complete-in-minibuffer)
      "C-S-h" #'embark-bindings
      "<escape>" #'keyboard-quit
      (:when (modulep! :cae ai)
       "<f5>" #'gptel-menu
       "<f6>" #'aider-transient-menu)
      "<f7>" #'eri/expand-region
      "C-<f1>" #'try
      (:map process-menu-mode-map
       "o" #'link-hint-open-link)
      (:after vertico
       (:map vertico-map
        "<prior>" #'vertico-scroll-down
        "<next>" #'vertico-scroll-up
        "C-z" #'cae-embark-act-with-completing-read)))
(define-key resize-window-repeat-map "_" #'shrink-window)

;; Resolve the age-old conflict between using TAB for completion,
;; indentation, and expanding snippets.
(map! [C-i] #'doom/dumb-indent
      "C-S-i" #'doom/dumb-dedent
      (:after cc-mode
       :map c-mode-base-map
       "TAB" nil))

;; Make YaSnippet TAB work in Org mode.
(when (modulep! :editor snippets)
  (add-hook! 'org-load-hook :append
    (defun cae-org-fix-keybindings ()
      (map! :map org-mode-map
            :ie [tab] nil)
      (yas-minor-mode +1))))

;; This one is because I bind `C-h' to a dedicated key on my keyboard.
(define-key help-map (kbd "SPC") #'cae-pop-mark)
(define-key help-map (kbd "DEL") #'cae-pop-mark)
;; This is so that `describe-key-briefly' is more convenient to use with the
;; `<f1>' prefix.
(define-key help-map (kbd "<f1>") #'describe-key-briefly)
(define-key help-map (kbd "<f2>") #'describe-key) ; `F1 F2' is easier than `C-h k'.

;; Doom Emacs unbinds the tutorial keybinding.
(define-key help-map (kbd "M-t") #'help-with-tutorial)

(after! diff-mode
  (map! :map diff-mode-map
        "q" #'kill-this-buffer))

;; Monkey fix `project.el' overriding the `C-x p' keybinding.
(when (modulep! :ui popup)
  (unless (boundp 'cae-fix-popup-other-keybinding-idle-timer)
    (defvar cae-fix-popup-other-keybinding-idle-timer nil)
    (defun cae-fix-popup-other-keybinding ()
      (define-key ctl-x-map "p" nil)
      (map! :map ctl-x-map
            "p" #'+popup/other))
    (setq cae-fix-popup-other-keybinding-idle-timer
          (run-with-idle-timer 3 t #'cae-fix-popup-other-keybinding))))

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


;;; Fixup leader key and C-c

(when (modulep! :editor snippets)
  (map! (:when (modulep! :completion vertico))
        :map yas-minor-mode-map
        "C-c & C-s" nil
        "C-c & C-n" nil
        "C-c & C-v" nil
        "C-c &" nil)
  (map! :leader
        (:prefix-map ("S" . "snippets")
         :desc "Insert Snippet" "s" #'yas-insert-snippet
         :desc "Create new snippet" "n" #'+snippets/new
         :desc "Visit Snippet file" "v" #'+snippets/edit
         :desc "Create new snippet alias" "a" #'+snippets/new-alias
         :desc "Find private snippet" "p" #'+snippets/find-private
         :desc "Find snippet for mode" "m" #'+snippets/find-for-current-mode
         :desc "Find snippet" "f" #'+snippets/find)))

(when (and (modulep! :checkers syntax)
           (not (modulep! :checkers syntax +flymake)))
  (after! flycheck
    (define-key flycheck-mode-map flycheck-keymap-prefix nil))
  (map! :leader
        (:prefix-map ("C" . "checkers")
         :desc "Check buffer" "c" #'flycheck-buffer
         :desc "Clear errors" "C" #'flycheck-clear
         :desc "Compile buffer" "C-c" #'flycheck-compile
         :desc "Next error" "n" #'flycheck-next-error
         :desc "Previous error" "p" #'flycheck-previous-error
         :desc "List errors" "l" #'flycheck-list-errors
         :desc "Copy errors as kill" "C-w" #'flycheck-copy-errors-as-kill
         :desc "Select checker" "s" #'flycheck-select-checker
         :desc "Describe checker" "?" #'flycheck-describe-checker
         :desc "Display error at point" "h" #'flycheck-display-error-at-point
         :desc "Explain error at point" "e" #'flycheck-explain-error-at-point
         :desc "Display local help" "H" #'display-local-help
         :desc "Flycheck manual" "i" #'flycheck-manual
         :desc "Flycheck version" "V" #'flycheck-version
         :desc "Verify setup" "v" #'flycheck-verify-setup
         :desc "Disable checker" "x" #'flycheck-disable-checker)))


;;; Extra which-key descriptions

;; Add some descriptions for built-in prefixes.
(fset 'ctl-x-map ctl-x-map)
(after! which-key
  (which-key-add-keymap-based-replacements search-map "h" "highlight")
  (which-key-add-keymap-based-replacements help-map "4" "other-window")
  (which-key-add-keymap-based-replacements help-map "dp" "packages")
  (which-key-add-keymap-based-replacements ctl-x-map "8e" "emoji")
  (after! helm-global-bindings
    (which-key-add-keymap-based-replacements helm-command-map "h" "help")
    (which-key-add-keymap-based-replacements helm-command-map "C-x r" "registers"))
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
    (which-key-add-keymap-based-replacements ctl-x-map "r" "register"))
  (pushnew!
   ;; I like having curly shorthands for common command prefixes. This makes
   ;; the`which-key' popup more legible for me.
   which-key-replacement-alist
   '(("" . "evilem--?motion-\\(.*\\)") . (nil . "ęm -\\1"))
   '(("" . "evil-avy-\\(.*\\)") . (nil . "ęa-\\1"))
   '(("" . "\\`+?evil[-:/]?\\(.*\\)") . (nil . "ę-\\1"))
   '(("" . "\\(?:special-\\)?lispy\\(?:ville\\)?-\\(.*\\)") . (nil . "ȴ-\\1"))
   '(("" . "doom[-/]\\(.*\\)") . (nil . "ȡ-\\1"))
   '(("" . "cae-\\(?:evil-\\|unpackaged-\\)?\\(.*\\)") . (nil . "ç-\\1"))
   '(("" . "cae-\\(?:avy-\\)\\(.*\\)") . (nil . "ça-\\1"))
   ;; For these, you can always tell what the command does without the prefix.
   '(("" . "tab-bar-\\(.*\\)") . (nil . "\\1"))
   '(("" . "winum-\\(.*\\)") . (nil . "\\1"))
   '(("" . "+workspace[-/]\\(.*\\)") . (nil . "\\1"))))


;;; Consult keybindings

(when (modulep! :completion vertico)
  (map! ;; C-x bindings (ctl-x-map)
   "C-x t b" #'consult-buffer-other-tab
   "C-x M-:" #'consult-complex-command  ;orig. repeat-complex-command
   ;; Custom C/M-# bindings for fast register access
   "C-#" #'consult-register-store
   "M-#" #'consult-register-load
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
   "M-s e" #'consult-isearch-history
   :map isearch-mode-map
   "M-e" #'consult-isearch-history      ;orig. isearch-edit-string
   "M-s e" #'consult-isearch-history    ;orig. isearch-edit-string
   "M-s l" #'consult-line
   "M-s L" #'consult-line-multi
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
    "M-s d" #'consult-fd
    "M-s r" #'consult-ripgrep
    "M-s c" #'consult-locate)
   [remap Info-search] #'consult-info
   "M-X" #'consult-mode-command)
  (map! :map help-map "TAB" #'consult-info)
  (when (modulep! :tools debugger +lsp)
    (after! dap-ui
      (map! :map dap-ui-repl-mode-map
            "M-r" #'consult-history)))

  ;; `M-p' is already bound to `previous-history-element'. This also has to be
  ;; set before loading `vertico-posframe'.
  (setq vertico-posframe-vertico-multiform-key "M-P"))

;;; Leader keybindings
(autoload 'lsp-ui-imenu "lsp-ui-imenu" nil t)
(map! :leader
      (:prefix "i"
       (:when (modulep! :completion vertico)
        :desc "Keyboard macro" "k" #'consult-kmacro))
      (:prefix "s"
       :desc "Copy link" "y" #'link-hint-copy-link)
      (:prefix "c"
       (:when (modulep! :tools editorconfig)
        :desc "Format whitespace" "C-f" #'editorconfig-format-buffer)
       :desc "Imenu sidebar" "TAB" #'lsp-ui-imenu
       (:prefix-map ("=" . "substitute")
        :desc "Substitute in defun" "d" #'substitute-target-in-defun
        :desc "Substitute in buffer" "b" #'substitute-target-in-buffer
        :desc "Substitute above point" "p" #'substitute-target-above-point
        :desc "Substitute below point" "n" #'substitute-target-below-point))
      (:prefix "t"
       :desc "Vertical line" "C-v" #'vline-mode
       :desc "Font lock mode" "C-f" #'font-lock-mode)
      (:prefix "f"
       :desc "Find sibling file" "TAB" #'cae-find-sibling-file
       (:when (modulep! :completion vertico)
        :desc "Find directory" "d" #'consult-dir))
      (:prefix "o"
       (:when (modulep! :app irc)
        :desc "Open ERC" "i" #'=irc)
       (:when (modulep! :cae notifications)
        :desc "Toggle notifications" "`" #'cae-ednc-toggle-notifications)
       (:when (modulep! :term eshell)
        :desc "Open eshell workspace" "C-e" #'cae-open-eshell-in-new-workspace)
       (:when (modulep! :term vterm)
        :desc "Open vterm workspace" "C-t" #'cae-open-vterm-in-new-workspace))
      (:prefix "TAB"
       :desc "Switch to 10th workspace" "0" #'cae-workspace-switch-to-9
       :desc "Switch to 11th workspace" "-" #'cae-workspace-switch-to-10
       :desc "Switch to final workspace" "=" #'+workspace/switch-to-final))
(after! which-key
  (setq which-key-replacement-alist
        (cl-remove-if (lambda (x)
                        (let ((cdr-x (cdr-safe (cdr x))))
                          (and (stringp cdr-x)
                               (string-equal cdr-x "Switch to final workspace")
                               (stringp (car-safe (car x)))
                               (string-suffix-p "0\\\\'" (caar x)))))
                      which-key-replacement-alist)))
(map! :map help-map
      (:prefix "d"
       :desc "Open Eshell alias file" "e" (cmd! () (find-file eshell-aliases-file))
       :desc "Toggle debug-on-quit" "q" (cmd! () (setq debug-on-quit (not debug-on-quit)))))
(which-key-add-keymap-based-replacements help-map "de" "Open Eshell alias file")
(which-key-add-keymap-based-replacements help-map "dq" "Toggle debug-on-quit")

;;; Other modules

;; This is also bound for Evil in `cae-evil.el'.
(after! org
  (map! :map org-mode-map
        (:when (not (modulep! :editor evil))
         "M-RET" #'org-insert-heading
         "M-S-RET" #'org-insert-todo-heading
         "M-<return>" #'org-insert-heading
         "M-S-<return>" #'org-insert-todo-heading)
        :localleader
        :desc "Insert heading" "RET" #'org-ctrl-c-ret))

(after! treemacs
  (when (modulep! :completion vertico)
    (map! :map treemacs-mode-map
          "j" #'consult-line)
    (when (modulep! :editor evil)
      (map! :map evil-treemacs-state-map
            "J" #'consult-line))))
