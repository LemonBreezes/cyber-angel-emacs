;;; private/misc-applications/+decide.el -*- lexical-binding: t; -*-

(use-package! decide
  :defer t
  :init
  (map! :leader
        (:prefix +misc-applications-insert-prefix
         (:prefix ("d" . "decide")
          "?" #'decide-dwim-insert
          "+" #'decide-for-me-likely
          "-" #'decide-for-me-unlikely
          "d" #'decide-roll-dice
          "D" #'decide-roll-2d6
          "3" #'decide-roll-1d3
          "4" #'decide-roll-1d4
          "5" #'decide-roll-1d5
          "6" #'decide-roll-1d6
          "7" #'decide-roll-1d7
          "8" #'decide-roll-1d8
          "9" #'decide-roll-1d9
          "1 0" #'decide-roll-1d10
          "1 2" #'decide-roll-1d12
          "2 0" #'decide-roll-1d20
          "%" #'decide-roll-1d100
          "f" #'decide-roll-fate
          "a" #'decide-roll-1dA
          "A" #'decide-roll-2dA
          "r" #'decide-random-range
          "c" #'decide-random-choice
          "t" #'decide-from-table
          (:prefix ("w" . "whereto compass")
           "4" #'decide-whereto-compass-4
           "6" #'decide-whereto-compass-6
           "8" #'decide-whereto-compass-8
           "1 0" #'decide-whereto-compass-10)
          (:prefix ("W" . "whereto relative")
           "2" #'decide-whereto-relative-2
           "3" #'decide-whereto-relative-3
           "4" #'decide-whereto-relative-4
           "6" 'decide-whereto-relative-6)
          "RET" #'decide-question-return
          "SPC" #'decide-question-space)))
  (autoload #'decide-dwim-insert "decide" nil t)
  (autoload #'decide-for-me-likely "decide" nil t)
  (autoload #'decide-for-me-unlikely "decide" nil t)
  (autoload #'decide-roll-dice "decide" nil t)
  (autoload #'decide-roll-2d6 "decide" nil t)
  (autoload #'decide-roll-1d3 "decide" nil t)
  (autoload #'decide-roll-1d4 "decide" nil t)
  (autoload #'decide-roll-1d5 "decide" nil t)
  (autoload #'decide-roll-1d6 "decide" nil t)
  (autoload #'decide-roll-1d7 "decide" nil t)
  (autoload #'decide-roll-1d8 "decide" nil t)
  (autoload #'decide-roll-1d9 "decide" nil t)
  (autoload #'decide-roll-1d10 "decide" nil t)
  (autoload #'decide-roll-1d12 "decide" nil t)
  (autoload #'decide-roll-1d20 "decide" nil t)
  (autoload #'decide-roll-1d100 "decide" nil t)
  (autoload #'decide-roll-fate "decide" nil t)
  (autoload #'decide-roll-1dA "decide" nil t)
  (autoload #'decide-roll-2dA "decide" nil t)
  (autoload #'decide-random-range "decide" nil t)
  (autoload #'decide-random-choice "decide" nil t)
  (autoload #'decide-from-table "decide" nil t)
  (autoload #'decide-whereto-compass-4 "decide" nil t)
  (autoload #'decide-whereto-compass-6 "decide" nil t)
  (autoload #'decide-whereto-compass-8 "decide" nil t)
  (autoload #'decide-whereto-compass-10 "decide" nil t)
  (autoload #'decide-whereto-relative-2 "decide" nil t)
  (autoload #'decide-whereto-relative-3 "decide" nil t)
  (autoload #'decide-whereto-relative-4 "decide" nil t)
  (autoload #'decide-whereto-relative-6 "decide" nil t)
  (autoload #'decide-question-return "decide" nil t)
  (autoload #'decide-question-space "decide" nil t)
  :config
  (map! :prefix +misc-applications-insert-prefix
        "d" #'decide-prefix-map))
