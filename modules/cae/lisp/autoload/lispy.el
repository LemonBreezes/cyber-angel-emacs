;;; cae/lisp/autoload/lispy.el -*- lexical-binding: t; -*-

(require 'transient)

(transient-define-prefix cae-lispy-cheatsheet ()
  "Lispy Cheatsheet"
  [
   ;; Group “Move”
   ["Move"
    ("l" "right" special-lispy-right)
    ("j" "down" special-lispy-down)
    ("k" "up" special-lispy-up)
    ("d" "different" special-lispy-different)
    ("y" "occur" special-lispy-occur)
    ("a" "ace-symbol" special-lispy-ace-symbol)
    ("g" "goto-local" special-lispy-goto-local)
    ("G" "goto" special-lispy-goto)
    ("F" "follow" special-lispy-follow)
    ("q" "ace-paren" special-lispy-ace-paren)
    (">" "slurp" special-lispy-slurp)
    ("<" "barf" special-lispy-barf)]
   ;; Group “Edit”
   ["Edit"
    ("P" "paste" special-lispy-paste)
    ("r" "raise" special-lispy-raise)
    ("R" "raise-some" special-lispy-raise-some)
    ("C" "convolute" special-lispy-convolute)
    ("X" "convolute←" special-lispy-convolute-left)
    ("w" "move-up" special-lispy-move-up)
    ("s" "move-down" special-lispy-move-down)
    ("O" "oneline" special-lispy-oneline)
    ("M" "multiline" special-lispy-alt-multiline)
    ("S" "stringify" special-lispy-stringify)
    ("i" "tab" special-lispy-tab)
    ("c" "clone" special-lispy-clone)
    ("t" "teleport" special-lispy-teleport)]
   ;; Group “Eval”
   ["Eval"
    ("p" "eval-other-window" special-lispy-eval-other-window)
    ("e" "eval" special-lispy-eval)
    ("E" "eval+insert" special-lispy-eval-and-insert)]
   ;; Group “Other”
   ["Other"
    ("h" "left" special-lispy-left)
    ("f" "flow" special-lispy-flow)
    ("o" "goto-mode" special-lispy-goto-mode)
    ("u" "undo" special-lispy-undo)
    ("x" "x" special-lispy-x)
    ("Z" "edebug-stop" special-lispy-edebug-stop)
    ("V" "visit" special-lispy-visit)
    ("." "repeat" special-lispy-repeat)
    (";" "comment" lispy-comment)
    ("+" "join" special-lispy-join)
    ("/" "splice" special-lispy-splice)
    ("~" "tilde" special-lispy-tilde)
    ("_" "underscore" special-lispy-underscore)
    ("'" "tick" lispy-tick)]
   ;; Group “Select”
   ["Select"
    ("m" "mark-list" special-lispy-mark-list)
    ("n" "new-copy" special-lispy-new-copy)
    ("-" "ace-subword" special-lispy-ace-subword)]
   ;; Group “View”
   ["View"
    ("I" "shifttab" special-lispy-shifttab)
    ("N" "narrow" special-lispy-narrow)
    ("W" "widen" special-lispy-widen)
    ("B" "ediff" special-lispy-ediff-regions)]
   ;; Group “Misc”
   ["Misc"
    ("J" "outline next" special-lispy-outline-next)
    ("K" "outline prev" special-lispy-outline-prev)
    ("L" "outline↓child" special-lispy-outline-goto-child)
    ("D" "pop-tag" special-pop-tag-mark)]
   ]
  )
