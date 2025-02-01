;;; cae/lisp/autoload/lispy.el -*- lexical-binding: t; -*-

;;;###autoload (autoload 'cae-lispy-cheatsheet/body "cae/lisp/autoload/lispy" nil t)
(let ((bindings
       '(("l" special-lispy-right "")
         ("h" special-lispy-left "")
         ("f" special-lispy-flow "")
         ("j" special-lispy-down "")
         ("k" special-lispy-up "")
         ("d" special-lispy-different "Move")
         ("o" special-lispy-goto-mode "Other")
         ("p" special-lispy-eval-other-window "Eval")
         ("P" special-lispy-paste "Edit")
         ("y" special-lispy-occur "Move")
         ("z" special-lh-knight/body "Move")
         ("J" special-lispy-outline-next "")
         ("K" special-lispy-outline-prev "")
         ("L" special-lispy-outline-goto-child "")
         ("r" special-lispy-raise "Edit")
         ("R" special-lispy-raise-some "Edit")
         ("C" special-lispy-convolute "Edit")
         ("X" special-lispy-convolute-left "Edit")
         ("w" special-lispy-move-up "Edit")
         ("s" special-lispy-move-down "Edit")
         ("O" special-lispy-oneline "Edit")
         ("M" special-lispy-alt-multiline "Edit")
         ("S" special-lispy-stringify "Edit")
         ("a" special-lispy-ace-symbol "Move")
         ("H" special-lispy-ace-symbol-replace "Edit")
         ("m" special-lispy-mark-list "Select")
         ("e" special-lispy-eval "Eval")
         ("E" special-lispy-eval-and-insert "Eval")
         ("g" special-lispy-goto-local "Move")
         ("G" special-lispy-goto "Move")
         ("F" special-lispy-follow "Move")
         ("D" special-pop-tag-mark "Move")
         ("A" special-lispy-beginning-of-defun "Move")
         ("i" special-lispy-tab "Edit")
         ("I" special-lispy-shifttab "View")
         ("N" special-lispy-narrow "View")
         ("W" special-lispy-widen "View")
         ("c" special-lispy-clone "Edit")
         ("u" special-lispy-undo "Other")
         ("q" special-lispy-ace-paren "Move")
         ("Q" special-lispy-ace-char "Move")
         ("v" special-lispy-view "View")
         ("t" special-lispy-teleport "Edit")
         ("n" special-lispy-new-copy "Select")
         ("b" special-lispy-back "Move")
         ("B" special-lispy-ediff-regions "View")
         ("x" special-lispy-x "Other")
         ("Z" special-lispy-edebug-stop "Other")
         ("V" special-lispy-visit "Other")
         (">" special-lispy-slurp "")
         ("<" special-lispy-barf "")
         ("." special-lispy-repeat "Other")
         (";" lispy-comment "")
         ("+" special-lispy-join "")
         ("/" special-lispy-splice "")
         ("-" special-lispy-ace-subword "Select")
         ("~" special-lispy-tilde "")
         ("_" special-lispy-underscore "")
         ("'" lispy-tick "")
         ("," nil ""))))
  (dolist (binding bindings)
    (define-key lispy-mode-map (car binding) (cadr binding)))
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
