;;; private/misc-applications/trash/+decide.el -*- lexical-binding: t; -*-

;; I never really saw a use for having this package in my config, it's a pretty
;; niche package. I might add it back in the future though.

(use-package! decide
  :defer t :init
  (map! :map +misc-applications-insert-map
        (:prefix "d"
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
         (:prefix "w"
          "4" #'decide-whereto-compass-4
          "6" #'decide-whereto-compass-6
          "8" #'decide-whereto-compass-8
          "1 0" #'decide-whereto-compass-10)
         (:prefix "W"
          "2" #'decide-whereto-relative-2
          "3" #'decide-whereto-relative-3
          "4" #'decide-whereto-relative-4
          "6" 'decide-whereto-relative-6)
         "RET" #'decide-question-return
         "SPC" #'decide-question-space))
  (after! which-key
    (which-key-add-keymap-based-replacements +misc-applications-insert-map
      "d" "decide"
      "dw" "whereto compass"
      "dW" "whereto relative"))
  (dolist (f '(decide-dwim-insert decide-for-me-likely
               decide-for-me-unlikely decide-roll-dice decide-roll-2d6
               decide-roll-1d3 decide-roll-1d4 decide-roll-1d5 decide-roll-1d6
               decide-roll-1d7 decide-roll-1d8 decide-roll-1d9 decide-roll-1d10
               decide-roll-1d12 decide-roll-1d20 decide-roll-1d100
               decide-roll-fate decide-roll-1dA decide-roll-2dA
               decide-random-range decide-random-choice decide-from-table
               decide-whereto-compass-4 decide-whereto-compass-6
               decide-whereto-compass-8 decide-whereto-compass-10
               decide-whereto-relative-2 decide-whereto-relative-3
               decide-whereto-relative-4 decide-whereto-relative-6
               decide-question-return decide-question-space))
    (autoload f "decide" nil t))
  :config
  (map! :prefix +misc-applications-insert-prefix
        "d" #'decide-prefix-map)
  (after! which-key
    (which-key-add-keymap-based-replacements decide-prefix-map
      "w" "whereto compass"
      "W" "whereto relative")))
