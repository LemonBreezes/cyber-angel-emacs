;;; startup/flameshot.el -*- lexical-binding: t; -*-

(defvar startup/flameshot-process nil)
(defvar startup/flameshot-executable (executable-find "flameshot"))

(defun startup/start-flameshot ()
  (when startup/flameshot-executable
    (setq startup/flameshot-process
          (start-process "flameshot"
                         " *startup/flameshot*"
                         startup/flameshot-executable))))

(defun startup/kill-flameshot ()
  (interactive)
  (when (process-live-p startup/flameshot-process)
    (kill-process startup/flameshot-process)))

(defun startup/restart-flameshot ()
  (interactive)
  (startup/kill-flameshot)
  (startup/start-flameshot))

(defun startup/flameshot-take-screenshot ()
  (interactive)
  (start-process "flameshot"
                 " *startup/flameshot-gui*"
                 startup/flameshot-executable
                 "gui"))

(map! :map +startup-applications-map
      :prefix "f"
      "r" #'startup/restart-flameshot
      "x" #'startup/kill-flameshot
      "f" #'startup/flameshot-take-screenshot)
(after! which-key
  (which-key-add-keymap-based-replacements +startup-applications-map
    "f" "Flameshot"
    "f r" "Restart Flameshot"
    "f x" "Kill Flameshot"
    "f f" "Take screenshot"))

;; (if (process-live-p startup/flameshot-process)
;;     (startup/restart-flameshot)
;;   (startup/start-flameshot))
