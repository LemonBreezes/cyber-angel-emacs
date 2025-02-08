;;; cae/lisp/autoload/lispy.el -*- lexical-binding: t; -*-

(require 'transient)

(defadvice! cae-lispy-cheatsheet-evil-insert-state-a ()
  :before #'cae-lispy-cheatsheet
  (when (and (featurep 'evil)
             (memq evil-state '(normal visual)))
    (evil-insert-state)))

;;;###autoload (autoload 'cae-lispy-cheatsheet "cae/lisp/autoload/lispy" nil t)
(transient-define-prefix cae-lispy-cheatsheet ()
  "Lispy Cheatsheet"
  ;; Group “Move”
  [["Move"
    ("l" "right" special-lispy-right :transient t)
    ("j" "down" special-lispy-down :transient t)
    ("k" "up" special-lispy-up :transient t)
    ("d" "different" special-lispy-different :transient t)
    ("y" "occur" special-lispy-occur :transient t)
    ("a" "ace-symbol" special-lispy-ace-symbol :transient t)
    ("g" "goto-local" special-lispy-goto-local :transient t)
    ("G" "goto" special-lispy-goto :transient t)
    ("F" "follow" special-lispy-follow :transient t)
    ("q" "ace-paren" special-lispy-ace-paren :transient t)
    (">" "slurp" special-lispy-slurp :transient t)
    ("<" "barf" special-lispy-barf :transient t)]
   ;; Group “Edit”
   ["Edit"
    ("P" "paste" special-lispy-paste :transient t)
    ("r" "raise" special-lispy-raise :transient t)
    ("R" "raise-some" special-lispy-raise-some :transient t)
    ("C" "convolute" special-lispy-convolute :transient t)
    ("X" "convolute←" special-lispy-convolute-left :transient t)
    ("w" "move-up" special-lispy-move-up :transient t)
    ("s" "move-down" special-lispy-move-down :transient t)
    ("O" "oneline" special-lispy-oneline :transient t)
    ("M" "multiline" special-lispy-alt-multiline :transient t)
    ("S" "stringify" special-lispy-stringify :transient t)
    ("i" "tab" special-lispy-tab :transient t)
    ("c" "clone" special-lispy-clone :transient t)
    ("t" "teleport" special-lispy-teleport :transient t)]
   ;; Group “Eval”
   ["Eval"
    ("p" "eval-other-window" special-lispy-eval-other-window :transient t)
    ("e" "eval" special-lispy-eval :transient t)
    ("E" "eval+insert" special-lispy-eval-and-insert :transient t)]
   ;; Group “Other”
   ["Other"
    ("h" "left" special-lispy-left :transient t)
    ("f" "flow" special-lispy-flow :transient t)
    ("o" "goto-mode" special-lispy-goto-mode :transient t)
    ("u" "undo" special-lispy-undo :transient t)
    ("x" "x" special-lispy-x :transient t)
    ("Z" "edebug-stop" special-lispy-edebug-stop :transient t)
    ("V" "visit" special-lispy-visit :transient t)
    ("." "repeat" special-lispy-repeat :transient t)
    (";" "comment" lispy-comment :transient t)
    ("+" "join" special-lispy-join :transient t)
    ("/" "splice" special-lispy-splice :transient t)
    ("~" "tilde" special-lispy-tilde :transient t)
    ("_" "underscore" special-lispy-underscore :transient t)
    ("'" "tick" lispy-tick :transient t)]
   ;; Group “Select”
   ["Select"
    ("m" "mark-list" special-lispy-mark-list :transient t)
    ("n" "new-copy" special-lispy-new-copy :transient t)
    ("-" "ace-subword" special-lispy-ace-subword :transient t)]
   ;; Group “View”
   ["View"
    ("I" "shifttab" special-lispy-shifttab :transient t)
    ("N" "narrow" special-lispy-narrow :transient t)
    ("W" "widen" special-lispy-widen :transient t)
    ("B" "ediff" special-lispy-ediff-regions :transient t)]
   ;; Group “Misc”
   ["Misc"
    ("J" "outline next" special-lispy-outline-next :transient t)
    ("K" "outline prev" special-lispy-outline-prev :transient t)
    ("L" "outline↓child" special-lispy-outline-goto-child :transient t)
    ("D" "pop-tag" special-pop-tag-mark :transient t)]])
