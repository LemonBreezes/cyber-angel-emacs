;;; ~/.doom.d/lisp/cae-keyboard.el -*- lexical-binding: t; -*-

;; I have a pretty complicated keyboard layout.

(map! "C-x t )" #'tab-close
      "C-x t #" #'tab-close-other
      "C-x t @" #'tab-new)
(after! which-key
  (add-to-list 'which-key-replacement-alist
               '(("C-x t )" . "tab-close") . t))
  (add-to-list 'which-key-replacement-alist
               '(("C-x t #" . "tab-close-other") . t))
  (add-to-list 'which-key-replacement-alist
               '(("C-x t @" . "tab-new") . t)))

(when (modulep! :ui workspaces)
  (map! :leader
      :prefix "w"
      "!" #'+workspace/switch-to-0
      "@" #'+workspace/switch-to-1
      ";" #'+workspace/switch-to-2
      "=" #'+workspace/switch-to-3
      "+" #'+workspace/switch-to-4
      "$" #'+workspace/switch-to-5
      "&" #'+workspace/switch-to-6
      "*" #'+workspace/switch-to-7
      "(" #'+workspace/switch-to-8
      ")" #'+workspace/switch-to-final)
(after! which-key
  (dotimes (i 9)
    (add-to-list 'which-key-replacement-alist
                 `((nil . ,(concat "+workspace/switch-to-" (number-to-string i))) . t)))
  (add-to-list 'which-key-replacement-alist
               '((nil . "+workspace/switch-to-final") . t))))

(defvar home-row-numbers-qwerty
  '(?r ?a ?e ?n ?s ?d ?o ?t ?i ?h)
  "list of the qwerty home row keys")

(defvar home-row-numbers-qwerty-numpad
  '(?c ?x ?\. ?o ?t ?i ?w ?l ?y ?\ )
  "keys forming a numpad under the right hand in qwerty")

(defvar home-row-numbers-norm
  '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)
  "list of the numbers on the keyboard in normal order")

(defvar home-row-numbers nil)

(home-row-numbers)
