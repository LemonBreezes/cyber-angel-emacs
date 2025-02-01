;;; cae/lisp/autoload/lispy.el -*- lexical-binding: t; -*-

(require 'transient)

(transient-define-prefix cae-lispy-cheatsheet ()
  "Lispy Cheatsheet"
  ;; Group “Move”
  [["Move"
    ("l" "right" special-lispy-right :exit nil)
    ("j" "down" special-lispy-down :exit nil)
    ("k" "up" special-lispy-up :exit nil)
    ("d" "different" special-lispy-different :exit nil)
    ("y" "occur" special-lispy-occur :exit nil)
    ("a" "ace-symbol" special-lispy-ace-symbol :exit nil)
    ("g" "goto-local" special-lispy-goto-local :exit nil)
    ("G" "goto" special-lispy-goto :exit nil)
    ("F" "follow" special-lispy-follow :exit nil)
    ("q" "ace-paren" special-lispy-ace-paren :exit nil)
    (">" "slurp" special-lispy-slurp :exit nil)
    ("<" "barf" special-lispy-barf :exit nil)]
   ;; Group “Edit”
   ["Edit"
    ("P" "paste" special-lispy-paste :exit nil)
    ("r" "raise" special-lispy-raise :exit nil)
    ("R" "raise-some" special-lispy-raise-some :exit nil)
    ("C" "convolute" special-lispy-convolute :exit nil)
    ("X" "convolute←" special-lispy-convolute-left :exit nil)
    ("w" "move-up" special-lispy-move-up :exit nil)
    ("s" "move-down" special-lispy-move-down :exit nil)
    ("O" "oneline" special-lispy-oneline :exit nil)
    ("M" "multiline" special-lispy-alt-multiline :exit nil)
    ("S" "stringify" special-lispy-stringify :exit nil)
    ("i" "tab" special-lispy-tab :exit nil)
    ("c" "clone" special-lispy-clone :exit nil)
    ("t" "teleport" special-lispy-teleport :exit nil)]
   ;; Group “Eval”
   ["Eval"
    ("p" "eval-other-window" special-lispy-eval-other-window :exit nil)
    ("e" "eval" special-lispy-eval :exit nil)
    ("E" "eval+insert" special-lispy-eval-and-insert :exit nil)]
   ;; Group “Other”
   ["Other"
    ("h" "left" special-lispy-left :exit nil)
    ("f" "flow" special-lispy-flow :exit nil)
    ("o" "goto-mode" special-lispy-goto-mode :exit nil)
    ("u" "undo" special-lispy-undo :exit nil)
    ("x" "x" special-lispy-x :exit nil)
    ("Z" "edebug-stop" special-lispy-edebug-stop :exit nil)
    ("V" "visit" special-lispy-visit :exit nil)
    ("." "repeat" special-lispy-repeat :exit nil)
    (";" "comment" lispy-comment :exit nil)
    ("+" "join" special-lispy-join :exit nil)
    ("/" "splice" special-lispy-splice :exit nil)
    ("~" "tilde" special-lispy-tilde :exit nil)
    ("_" "underscore" special-lispy-underscore :exit nil)
    ("'" "tick" lispy-tick :exit nil)]
   ;; Group “Select”
   ["Select"
    ("m" "mark-list" special-lispy-mark-list :exit nil)
    ("n" "new-copy" special-lispy-new-copy :exit nil)
    ("-" "ace-subword" special-lispy-ace-subword :exit nil)]
   ;; Group “View”
   ["View"
    ("I" "shifttab" special-lispy-shifttab :exit nil)
    ("N" "narrow" special-lispy-narrow :exit nil)
    ("W" "widen" special-lispy-widen :exit nil)
    ("B" "ediff" special-lispy-ediff-regions :exit nil)]
   ;; Group “Misc”
   ["Misc"
    ("J" "outline next" special-lispy-outline-next :exit nil)
    ("K" "outline prev" special-lispy-outline-prev :exit nil)
    ("L" "outline↓child" special-lispy-outline-goto-child :exit nil)
    ("D" "pop-tag" special-pop-tag-mark :exit nil)]])
