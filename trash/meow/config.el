;;; editor/meow/config.el -*- lexical-binding: t; -*-

(eval-when-compile
  (when (autoloadp (symbol-function #'cae-keyboard-kbd))
    (autoload-do-load (symbol-function #'cae-keyboard-kbd))))

;; Setup Functions
(advice-add #'meow-digit-argument :before #'cae-meow-use-keyboard-layout-a)

;; Leader Key
(defun meow/setup-leader ()
  (eval
   `(map! :leader
          "?" #'meow-cheatsheet
          "/" #'meow-keypad-describe-key
          ,(cae-keyboard-kbd "1") #'meow-digit-argument
          ,(cae-keyboard-kbd "2") #'meow-digit-argument
          ,(cae-keyboard-kbd "3") #'meow-digit-argument
          ,(cae-keyboard-kbd "4") #'meow-digit-argument
          ,(cae-keyboard-kbd "5") #'meow-digit-argument
          ,(cae-keyboard-kbd "6") #'meow-digit-argument
          ,(cae-keyboard-kbd "7") #'meow-digit-argument
          ,(cae-keyboard-kbd "8") #'meow-digit-argument
          ,(cae-keyboard-kbd "9") #'meow-digit-argument
          ,(cae-keyboard-kbd "0") #'meow-digit-argument)
   t)
  (after! which-key
    ;; Remove `meow-digit-argument' from the which-key menu.
    (push 'which-key-replacement-alist
          '((nil . "meow-digit-argument") . t))))

;; Keypad
(defun meow/setup-keypad ()
  (eval
   `(map! :map meow-keypad-state-keymap
          "?" #'meow-cheatsheet
          "/" #'meow-keypad-describe-key
          ;; Add digit arguments?
          "h" #'help-command))
  t)

(defconst meow-cheatsheet-layout-cae
  `((<TLDE> "`" "~")
    (<AE01> "1" "!")
    (<AE02> "2" "@")
    (<AE03> "3" "#")
    (<AE04> "4" "$")
    (<AE05> "5" "%")
    (<AE06> "6" "^")
    (<AE07> "7" "&")
    (<AE08> "8" "*")
    (<AE09> "9" "(")
    (<AE10> "0" ")")
    (<AE11> "-" "_")
    (<AE12> "=" "+")
    (<AD01> ,(cae-keyboard-kbd "q") ,(cae-keyboard-kbd "Q"))
    (<AD02> ,(cae-keyboard-kbd "w") ,(cae-keyboard-kbd "W"))
    (<AD03> ,(cae-keyboard-kbd "e") ,(cae-keyboard-kbd "E"))
    (<AD04> ,(cae-keyboard-kbd "r") ,(cae-keyboard-kbd "R"))
    (<AD05> ,(cae-keyboard-kbd "t") ,(cae-keyboard-kbd "T"))
    (<AD06> ,(cae-keyboard-kbd "y") ,(cae-keyboard-kbd "Y"))
    (<AD07> ,(cae-keyboard-kbd "u") ,(cae-keyboard-kbd "U"))
    (<AD08> ,(cae-keyboard-kbd "i") ,(cae-keyboard-kbd "I"))
    (<AD09> ,(cae-keyboard-kbd "o") ,(cae-keyboard-kbd "O"))
    (<AD10> ,(cae-keyboard-kbd "p") ,(cae-keyboard-kbd "P"))
    (<AD11> "[" "{")
    (<AD12> "]" "}")
    (<AC01> ,(cae-keyboard-kbd "a") ,(cae-keyboard-kbd "A"))
    (<AC02> ,(cae-keyboard-kbd "s") ,(cae-keyboard-kbd "S"))
    (<AC03> ,(cae-keyboard-kbd "d") ,(cae-keyboard-kbd "D"))
    (<AC04> ,(cae-keyboard-kbd "f") ,(cae-keyboard-kbd "F"))
    (<AC05> ,(cae-keyboard-kbd "g") ,(cae-keyboard-kbd "G"))
    (<AC06> ,(cae-keyboard-kbd "h") ,(cae-keyboard-kbd "H"))
    (<AC07> ,(cae-keyboard-kbd "j") ,(cae-keyboard-kbd "J"))
    (<AC08> ,(cae-keyboard-kbd "k") ,(cae-keyboard-kbd "K"))
    (<AC09> ,(cae-keyboard-kbd "l") ,(cae-keyboard-kbd "L"))
    (<AC10> ,(cae-keyboard-kbd ";") ,(cae-keyboard-kbd ":"))
    (<AC11> ,(cae-keyboard-kbd "'") ,(cae-keyboard-kbd "\""))
    (<AB01> ,(cae-keyboard-kbd "z") ,(cae-keyboard-kbd "Z"))
    (<AB02> ,(cae-keyboard-kbd "x") ,(cae-keyboard-kbd "X"))
    (<AB03> ,(cae-keyboard-kbd "c") ,(cae-keyboard-kbd "C"))
    (<AB04> ,(cae-keyboard-kbd "v") ,(cae-keyboard-kbd "V"))
    (<AB05> ,(cae-keyboard-kbd "b") ,(cae-keyboard-kbd "B"))
    (<AB06> ,(cae-keyboard-kbd "n") ,(cae-keyboard-kbd "N"))
    (<AB07> ,(cae-keyboard-kbd "m") ,(cae-keyboard-kbd "M"))
    (<AB08> ,(cae-keyboard-kbd ",") ,(cae-keyboard-kbd "<"))
    (<AB09> ,(cae-keyboard-kbd ".") ,(cae-keyboard-kbd ">"))
    (<AB10> ,(cae-keyboard-kbd "/") ,(cae-keyboard-kbd "?"))
    (<BKSL> "\\" "|")))

(defun meow/setup-cae ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-cae)
  (when (modulep! +override)
    (meow-motion-overwrite-define-key)) ;custom keybinding for motion state
  (eval
   `(map! :map meow-normal-state-keymap
          ,(cae-keyboard-kbd "1") #'meow-expand-1
          ,(cae-keyboard-kbd "2") #'meow-expand-2
          ,(cae-keyboard-kbd "3") #'meow-expand-3
          ,(cae-keyboard-kbd "4") #'meow-expand-4
          ,(cae-keyboard-kbd "5") #'meow-expand-5
          ,(cae-keyboard-kbd "6") #'meow-expand-6
          ,(cae-keyboard-kbd "7") #'meow-expand-7
          ,(cae-keyboard-kbd "8") #'meow-expand-8
          ,(cae-keyboard-kbd "9") #'meow-expand-9
          ,(cae-keyboard-kbd "0") #'meow-expand-0
          ":" #'meow-reverse            ;: -> ;
          "?" #'meow-cheatsheet
          "[" #'meow-beginning-of-thing
          "]" #'meow-end-of-thing
          "<" #'meow-beginning-of-thing
          ">" #'meow-end-of-thing
          "a" #'meow-append
          "A" #'meow-open-below
          "w" #'meow-back-word          ;b -> w
          "W" #'meow-back-symbol        ;B -> W
          "c" #'meow-change
          "d" #'meow-delete
          "D" #'meow-backward-delete
          "e" #'meow-line
          "E" #'meow-goto-line
          "t" #'meow-find               ;f -> t
          "g" #'meow-cancel-selection
          "G" #'meow-grab
          "b" #'meow-left               ;h -> b
          "B" #'meow-left-expand        ;H -> B
          "I" #'meow-open-above
          "i" #'meow-insert
          "j" #'meow-join
          "k" #'meow-kill
          "l" #'meow-till
          "m" #'meow-mark-word
          "M" #'meow-mark-symbol
          "n" #'meow-next
          "N" #'meow-next-expand
          "o" #'meow-block
          "O" #'meow-to-block
          "p" #'meow-prev
          "P" #'meow-prev-expand
          "q" #'meow-quit
          "Q" #'meow-goto-line
          "r" #'meow-replace
          "R" #'meow-swap-grab
          "s" #'meow-search
          "f" #'meow-right              ;t -> f
          "F" #'meow-right-expand       ;T -> F
          "u" #'meow-undo
          "U" #'meow-undo-in-selection
          "_" #'undo-fu-only-redo
          "v" #'meow-visit
          "h" #'meow-next-word          ;w -> h
          "H" #'meow-next-symbol        ;W -> H
          "x" #'meow-save
          "X" #'meow-sync-grab
          "y" #'meow-yank
          "z" #'meow-pop-selection
          "," #'meow-inner-of-thing
          "." #'meow-bounds-of-thing
          "-" #'negative-argument
          "'" #'repeat
          ;; commands not bound by default
          "Z" #'meow-pop-all-selection
          ;; ,(cae-keyboard-kbd "&") #'meow-query-replace
          ;; "%" #'meow-query-replace-regexp
          "Y" #'meow-yank-pop
          "\\" #'quoted-insert
          "S" #'embrace-commander)
   t))

(use-package! meow
  :init
  (add-hook 'doom-after-modules-config-hook #'meow-global-mode)
  :config
  (when (modulep! :emacs vc)
    (add-hook! 'git-timemachine-mode-hook
      (defun cae-meow-enable-motion-mode-after-delay-h ()
        (run-at-time 0.01 nil #'meow-motion-mode +1))))
  (meow/setup-cae)
  (meow/setup-keypad)
  (add-hook 'minions-mode-hook #'meow-setup-indicator)
  (setq meow-keypad-start-keys
        `((?x . ?x)
          (?h . ?h)
          (?c . ?c)
          (?\; . ?\;)                   ;For embark
          (?\s . ?\s)                   ;For pop mark
          ;; For popups
          (,(cae-keyboard-remap-char ?`) . ,(cae-keyboard-remap-char ?`))
          (,(cae-keyboard-remap-char ?~) . ,(cae-keyboard-remap-char ?~))))
  (dolist (p '((helpfulmode . normal)
               (Man-mode . normal)
               (message-buffer-mode . normal)
               (eshell-mode . insert)
               (eat-mode . insert)
               (vterm-mode . insert)
               (helpful-mode . motion)
               (gdb-frames-mode . normal)
               (Info-mode . insert)
               (dap-ui-repl-mode . insert)
               (inferior-emacs-lisp-mode . insert)
               (comint-mode . insert)
               (term-mode . insert)))
    (setf (alist-get (car p) meow-mode-state-list) (cdr p)))
  (setq meow-selection-command-fallback
        '((meow-change . meow-change-char)
          (meow-kill . meow-C-k)
          (meow-cancel-selection . keyboard-quit)
          (meow-pop-selection . meow-pop-grab)
          (meow-beacon-change . meow-beacon-change-char)
          ;; These are the ones that are not bound by default.
          (meow-reverse . exchange-point-and-mark)
          (meow-replace . back-to-indentation)
          (meow-save . cae-meow-save-line)))
  (when (modulep! :completion corfu)
    (after! corfu
      (add-hook 'meow-insert-exit-hook #'corfu-quit)))
  (unless (cae-display-graphic-p)       ;Make Meow usable in the terminal.
    (setq meow-esc-delay 0.001)
    (meow-esc-mode +1))
  (setq meow-use-clipboard t
        meow-select-on-change t
        meow-expand-hint-remove-delay 1.2 ;Just give me a little more time.
        meow-keypad-self-insert-undefined nil
        meow-use-enhanced-selection-effect t ;???
        meow-use-cursor-position-hack t      ;???
        meow-goto-line-function #'consult-goto-line
        meow-grab-fill-commands '(meow-query-replace meow-query-replace-regexp
                                  eval-expression pp-eval-expression))

  (map! :map meow-keymap [remap describe-key] #'helpful-key)
  ;; Ensure the cursor is updated when running Emacs in a terminal.
  (autoload 'etcc--evil-set-cursor "evil-terminal-cursor-changer")
  (advice-add #'meow--update-cursor :after #'cae-meow-update-cursor-a)
  (advice-add #'eldoc-print-current-symbol-info :after #'cae-meow-update-cursor-a))

(after! embrace
  (setq embrace-default-pairs
        '((?r . ("(" . ")"))
          (?R . ("( " . " )"))
          (?c . ("{" . "}"))
          (?C . ("{ " . " }"))
          (?\[ . ("[" . "]"))
          (?\] . ("[ " . " ]"))
          (?a . ("<" . ">"))
          (?A . ("< " . " >"))
          (?s . ("\"" . "\""))
          (?\' . ("\'" . "\'"))
          (?` . ("`" . "`")))))

;;Local Variables:
;;eval: (unless (modulep! :cae meow) (remove-hook 'write-file-functions #'eval-buffer t))
;;End:
