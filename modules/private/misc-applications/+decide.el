;;; private/misc-applications/+decide.el -*- lexical-binding: t; -*-

(use-package! decide
  :defer t
  :init
  (map! :prefix ("d" . "decide")
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
        "w 4" #'decide-whereto-compass-4
        "w 6" #'decide-whereto-compass-6
        "w 8" #'decide-whereto-compass-8
        "w 1 0" #'decide-whereto-compass-10
        "W 2" #'decide-whereto-relative-2
        "W 3" #'decide-whereto-relative-3
        "W 4" #'decide-whereto-relative-4
        "W 6" 'decide-whereto-relative-6
        "RET" #'decide-question-return
        "SPC" #'decide-question-space))
