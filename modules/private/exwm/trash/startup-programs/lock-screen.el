;;; startup/lock-screen.el -*- lexical-binding: t; -*-

(defun startup/lock-screen ()
  (interactive)
  "Lock screen using (zone) and xtrlock
 calls M-x zone on all frames and runs xtrlock"
  (interactive)
  (require 'zone)
  (save-excursion
    (set-process-sentinel
     (start-process "xtrlock" nil "xtrlock")
     (lambda (process event)
       (zone-leave-me-alone)))
    (run-with-idle-timer 1 nil #'zone-nyan-preview)))

(map! :map +startup-applications-map
      "l" #'startup/lock-screen)
(after! which-key
  (which-key-add-keymap-based-replacements +startup-applications-map
    "l" "Lock screen"))
