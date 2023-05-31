;;; editor/meow/config.el -*- lexical-binding: t; -*-

;; Setup Functions

;; Leader Key
(defun meow/setup-leader ()
 (map! :leader
  "?" #'meow-cheatsheet
  "/" #'meow-keypad-describe-key
  "1" #'meow-digit-argument
  "2" #'meow-digit-argument
  "3" #'meow-digit-argument
  "4" #'meow-digit-argument
  "5" #'meow-digit-argument
  "6" #'meow-digit-argument
  "7" #'meow-digit-argument
  "8" #'meow-digit-argument
  "9" #'meow-digit-argument
  "0" #'meow-digit-argument))

;; Keypad
(defun meow/setup-keypad ()
 (map! :map meow-keypad-state-keymap
  "?" #'meow-cheatsheet
  "/" #'meow-keypad-describe-key
  "1" #'meow-digit-argument
  "2" #'meow-digit-argument
  "3" #'meow-digit-argument
  "4" #'meow-digit-argument
  "5" #'meow-digit-argument
  "6" #'meow-digit-argument
  "7" #'meow-digit-argument
  "8" #'meow-digit-argument
  "9" #'meow-digit-argument
  "0" #'meow-digit-argument
  "h" #'help-command))

;; applies to all layouts (except dvp)
(defun meow/setup ()
  (map! :map meow-normal-state-keymap
   "0" #'meow-expand-0
   "1" #'meow-expand-1
   "2" #'meow-expand-2
   "3" #'meow-expand-3
   "4" #'meow-expand-4
   "5" #'meow-expand-5
   "6" #'meow-expand-6
   "7" #'meow-expand-7
   "8" #'meow-expand-8
   "9" #'meow-expand-9
   "-" #'negative-argument
   ";" #'meow-reverse
   "," #'meow-inner-of-thing
   "." #'meow-bounds-of-thing
   "'" #'repeat
   "<escape>" #'ignore))

;; Colemak
(defun meow/setup-colemak ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak)
  (meow/setup)
  (when (modulep! +override)
   (meow-motion-overwrite-define-key
    ;; Use e to move up, n to move down.
    ;; Since special modes usually use n to move down, we only overwrite e here.
    '("e" . meow-prev))
   (when (featurep! :editor meow +leader)
         (meow-motion-overwrite-define-key '("\\ e" . "H-e")))
   (meow-leader-define-key '("e" . "H-e")))
  (map! :map meow-normal-state-keymap
   "[" #'meow-beginning-of-thing
   "]" #'meow-end-of-thing
   "/" #'meow-visit
   "a" #'meow-append
   "A" #'meow-open-below
   "b" #'meow-back-word
   "B" #'meow-back-symbol
   "c" #'meow-change
   "d" #'meow-delete
   "e" #'meow-prev
   "E" #'meow-prev-expand
   "f" #'meow-find
   "g" #'meow-cancel-selection
   "G" #'meow-grab
   "h" #'meow-left
   "H" #'meow-left-expand
   "i" #'meow-right
   "I" #'meow-right-expand
   "j" #'meow-join
   "k" #'meow-kill
   "l" #'meow-line
   "L" #'meow-goto-line
   "m" #'meow-mark-word
   "M" #'meow-mark-symbol
   "n" #'meow-next
   "N" #'meow-next-expand
   "o" #'meow-block
   "O" #'meow-to-block
   "p" #'meow-yank
   "q" #'meow-quit
   "r" #'meow-replace
   "s" #'meow-insert
   "S" #'meow-open-above
   "t" #'meow-till
   "u" #'meow-undo
   "U" #'meow-undo-in-selection
   "v" #'meow-search
   "w" #'meow-next-word
   "W" #'meow-next-symbol
   "x" #'meow-delete
   "X" #'meow-backward-delete
   "y" #'meow-save
   "z" #'meow-pop-selection))

;; Dvorak
(defun meow/setup-dvorak ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvorak)
  (when (modulep! +override)
   (meow-motion-overwrite-define-key)) ; custom keybinding for motion state
  (meow/setup)
  (map! :map meow-normal-state-keymap
   "<" #'meow-beginning-of-thing
   ">" #'meow-end-of-thing
   "a" #'meow-append
   "A" #'meow-open-below
   "b" #'meow-back-word
   "B" #'meow-back-symbol
   "c" #'meow-change
   "d" #'meow-delete
   "D" #'meow-backward-delete
   "e" #'meow-line
   "E" #'meow-goto-line
   "f" #'meow-find
   "g" #'meow-cancel-selection
   "G" #'meow-grab
   "h" #'meow-left
   "H" #'meow-left-expand
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
   "t" #'meow-right
   "T" #'meow-right-expand
   "u" #'meow-undo
   "U" #'meow-undo-in-selection
   "v" #'meow-visit
   "w" #'meow-next-word
   "W" #'meow-next-symbol
   "x" #'meow-save
   "X" #'meow-sync-grab
   "y" #'meow-yank
   "z" #'meow-pop-selection))

;; Qwerty
(defun meow/setup-qwerty ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow/setup)
  (when (featurep! :editor meow +override)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev))
    (when (featurep! :editor meow +leader)
      (meow-motion-overwrite-define-key
       '("\\ j" . "H-j")
       '("\\ k" . "H-k")))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")))
  (map! :map meow-normal-state-keymap
        "[" #'meow-beginning-of-thing
        "]" #'meow-end-of-thing
        "a" #'meow-append
        "A" #'meow-open-below
        "b" #'meow-back-word
        "B" #'meow-back-symbol
        "c" #'meow-change
        "d" #'meow-delete
        "D" #'meow-backward-delete
        "e" #'meow-next-word
        "E" #'meow-next-symbol
        "f" #'meow-find
        "g" #'meow-cancel-selection
        "G" #'meow-grab
        "h" #'meow-left
        "H" #'meow-left-expand
        "i" #'meow-insert
        "I" #'meow-open-above
        "j" #'meow-next
        "J" #'meow-next-expand
        "k" #'meow-prev
        "K" #'meow-prev-expand
        "l" #'meow-right
        "L" #'meow-right-expand
        "m" #'meow-join
        "n" #'meow-search
        "o" #'meow-block
        "O" #'meow-to-block
        "p" #'meow-yank
        "q" #'meow-quit
        "Q" #'meow-goto-line
        "r" #'meow-replace
        "R" #'meow-swap-grab
        "s" #'meow-kill
        "t" #'meow-till
        "u" #'meow-undo
        "U" #'meow-undo-in-selection
        "v" #'meow-visit
        "w" #'meow-mark-word
        "W" #'meow-mark-symbol
        "x" #'meow-line
        "X" #'meow-goto-line
        "y" #'meow-save
        "Y" #'meow-sync-grab
        "z" #'meow-pop-selection))

;; Programmer Dvorak
(defun meow/setup-dvp ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvp)
  (when (modulep! +override)
    (meow-motion-overwrite-define-key)) ; custom keybinding for motion state
  (map! :map meow-normal-state-keymap
        "?" #'meow-cheatsheet
        "*" #'meow-expand-0
        "=" #'meow-expand-9
        "!" #'meow-expand-8
        "[" #'meow-expand-7
        "]" #'meow-expand-6
        "{" #'meow-expand-5
        "+" #'meow-expand-4
        "}" #'meow-expand-3
        ")" #'meow-expand-2
        "(" #'meow-expand-1
        "1" #'digit-argument
        "2" #'digit-argument
        "3" #'digit-argument
        "4" #'digit-argument
        "5" #'digit-argument
        "6" #'digit-argument
        "7" #'digit-argument
        "8" #'digit-argument
        "9" #'digit-argument
        "0" #'digit-argument
        "-" #'negative-argument
        ";" #'meow-reverse
        "," #'meow-inner-of-thing
        "." #'meow-bounds-of-thing
        "<" #'meow-beginning-of-thing
        ">" #'meow-end-of-thing
        "a" #'meow-append
        "A" #'meow-open-below
        "b" #'meow-back-word
        "B" #'meow-back-symbol
        "c" #'meow-change
        "d" #'meow-delete
        "D" #'meow-backward-delete
        "e" #'meow-line
        "E" #'meow-goto-line
        "f" #'meow-find
        "g" #'meow-cancel-selection
        "G" #'meow-grab
        "h" #'meow-left
        "H" #'meow-left-expand
        "i" #'meow-insert
        "I" #'meow-open-above
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
        "r" #'meow-replace
        "R" #'meow-swap-grab
        "s" #'meow-search
        "t" #'meow-right
        "T" #'meow-right-expand
        "u" #'meow-undo
        "U" #'meow-undo-in-selection
        "v" #'meow-visit
        "w" #'meow-next-word
        "W" #'meow-next-symbol
        "x" #'meow-save
        "X" #'meow-sync-grab
        "y" #'meow-yank
        "z" #'meow-pop-selection
        "'" #'repeat
        "<escape>" #'ignore))

(defconst meow-cheatsheet-layout-cae
  `((<TLDE> "`"	"~")
    (<AE01> "1"	"!")
    (<AE02> "2"	"@")
    (<AE03> "3"	"#")
    (<AE04> "4"	"$")
    (<AE05> "5"	"%")
    (<AE06> "6"	"^")
    (<AE07> "7"	"&")
    (<AE08> "8"	"*")
    (<AE09> "9"	"(")
    (<AE10> "0"	")")
    (<AE11> "-"	"_")
    (<AE12> "="	"+")
    (<AD01> ,(cae-keyboard-kbd "q")	,(cae-keyboard-kbd "Q"))
    (<AD02> ,(cae-keyboard-kbd "w")	,(cae-keyboard-kbd "W"))
    (<AD03> ,(cae-keyboard-kbd "e")	,(cae-keyboard-kbd "E"))
    (<AD04> ,(cae-keyboard-kbd "r")	,(cae-keyboard-kbd "R"))
    (<AD05> ,(cae-keyboard-kbd "t")	,(cae-keyboard-kbd "T"))
    (<AD06> ,(cae-keyboard-kbd "y")	,(cae-keyboard-kbd "Y"))
    (<AD07> ,(cae-keyboard-kbd "u")	,(cae-keyboard-kbd "U"))
    (<AD08> ,(cae-keyboard-kbd "i")	,(cae-keyboard-kbd "I"))
    (<AD09> ,(cae-keyboard-kbd "o")	,(cae-keyboard-kbd "O"))
    (<AD10> ,(cae-keyboard-kbd "p")	,(cae-keyboard-kbd "P"))
    (<AD11> "["	"{")
    (<AD12> "]"	"}")
    (<AC01> ,(cae-keyboard-kbd "a")	,(cae-keyboard-kbd "A"))
    (<AC02> ,(cae-keyboard-kbd "s")	,(cae-keyboard-kbd "S"))
    (<AC03> ,(cae-keyboard-kbd "d")	,(cae-keyboard-kbd "D"))
    (<AC04> ,(cae-keyboard-kbd "f")	,(cae-keyboard-kbd "F"))
    (<AC05> ,(cae-keyboard-kbd "g")	,(cae-keyboard-kbd "G"))
    (<AC06> ,(cae-keyboard-kbd "h")	,(cae-keyboard-kbd "H"))
    (<AC07> ,(cae-keyboard-kbd "j")	,(cae-keyboard-kbd "J"))
    (<AC08> ,(cae-keyboard-kbd "k")	,(cae-keyboard-kbd "K"))
    (<AC09> ,(cae-keyboard-kbd "l")	,(cae-keyboard-kbd "L"))
    (<AC10> ,(cae-keyboard-kbd ";")	,(cae-keyboard-kbd ":"))
    (<AC11> ,(cae-keyboard-kbd "'")	,(cae-keyboard-kbd "\""))
    (<AB01> ,(cae-keyboard-kbd "z")	,(cae-keyboard-kbd "Z"))
    (<AB02> ,(cae-keyboard-kbd "x")	,(cae-keyboard-kbd "X"))
    (<AB03> ,(cae-keyboard-kbd "c")	,(cae-keyboard-kbd "C"))
    (<AB04> ,(cae-keyboard-kbd "v")	,(cae-keyboard-kbd "V"))
    (<AB05> ,(cae-keyboard-kbd "b")	,(cae-keyboard-kbd "B"))
    (<AB06> ,(cae-keyboard-kbd "n")	,(cae-keyboard-kbd "N"))
    (<AB07> ,(cae-keyboard-kbd "m")	,(cae-keyboard-kbd "M"))
    (<AB08> ,(cae-keyboard-kbd ",")	,(cae-keyboard-kbd "<"))
    (<AB09> ,(cae-keyboard-kbd ".")	,(cae-keyboard-kbd ">"))
    (<AB10> ,(cae-keyboard-kbd "/")	,(cae-keyboard-kbd "?"))
    (<BKSL> "\\" "|")))

(defun meow/setup-cae ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-cae)
  (when (modulep! +override)
    (meow-motion-overwrite-define-key)) ; custom keybinding for motion state
  (meow/setup)
  (eval `(map! :map meow-normal-state-keymap
               "1" #'digit-argument
               "2" #'digit-argument
               "3" #'digit-argument
               "4" #'digit-argument
               "5" #'digit-argument
               "6" #'digit-argument
               "7" #'digit-argument
               "8" #'digit-argument
               "9" #'digit-argument
               "0" #'digit-argument
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
               ":" #'meow-reverse                  ; : -> ;
               "?" #'meow-cheatsheet
               "<" #'meow-beginning-of-thing
               ">" #'meow-end-of-thing
               "a" #'meow-append
               "A" #'meow-open-below
               "b" #'meow-back-word
               "B" #'meow-back-symbol
               "c" #'meow-change
               "d" #'meow-delete
               "D" #'meow-backward-delete
               "e" #'meow-line
               "E" #'meow-goto-line
               "f" #'meow-find
               "g" #'meow-cancel-selection
               "G" #'meow-grab
               "o" #'meow-left                     ; h -> o
               "O" #'meow-left-expand              ; H -> O
               "I" #'meow-open-above
               "i" #'meow-insert
               "j" #'meow-join
               "k" #'meow-kill
               "l" #'meow-till
               "m" #'meow-mark-word
               "M" #'meow-mark-symbol
               "n" #'meow-next
               "N" #'meow-next-expand
               "h" #'meow-block                    ; o -> h
               "H" #'meow-to-block                 ; O -> H
               "p" #'meow-prev
               "P" #'meow-prev-expand
               "q" #'meow-quit
               "Q" #'meow-goto-line
               "r" #'meow-replace
               "R" #'meow-swap-grab
               "s" #'meow-search
               "t" #'meow-right
               "T" #'meow-right-expand
               "u" #'meow-undo
               "U" #'meow-undo-in-selection
               "v" #'meow-visit
               "w" #'meow-next-word
               "W" #'meow-next-symbol
               "x" #'meow-save
               "X" #'meow-sync-grab
               "y" #'meow-yank
               "z" #'meow-pop-selection)))

(use-package! meow
  :init
  (add-hook 'doom-after-modules-config-hook #'meow-global-mode)
  :demand t
  :config
  (cond
   ((modulep! +colemak) (meow/setup-colemak))
   ((modulep! +dvorak) (meow/setup-dvorak))
   ((modulep! +qwerty) (meow/setup-qwerty))
   ((modulep! +dvp) (meow/setup-dvp))
   ((modulep! +cae) (meow/setup-cae)))
  (cond
   ((modulep! +leader)
    (map! :map meow-normal-state-keymap
          doom-leader-key doom-leader-map)
    (map! :map meow-motion-state-keymap
          doom-leader-key doom-leader-map)
    (map! :map meow-beacon-state-keymap
          doom-leader-key nil)
    (meow/setup-leader))
   (t (meow/setup-keypad)))
  (map! :map meow-keymap [remap describe-key] #'helpful-key))
