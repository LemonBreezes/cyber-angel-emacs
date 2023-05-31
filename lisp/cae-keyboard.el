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
