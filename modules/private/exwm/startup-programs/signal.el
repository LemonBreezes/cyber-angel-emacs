;;; startup/signal.el -*- lexical-binding: t; -*-

(defvar startup/signal-process nil)
(defvar startup/signal-executable (executable-find "signal-desktop"))
(defvar startup/signal-workspace "Signal")

(defun startup/start-signal (&optional arg)
  (when startup/signal-executable
    (setq startup/signal-process
          (start-process "signal"
                         " *startup/signal*"
                         startup/signal-executable
                         (if (eq (user-uid) 0) "--no-sandbox" "")))))

(defun startup/kill-signal (&optional arg)
  (interactive "p")
  (when (process-live-p startup/signal-process)
    (kill-process startup/signal-process)))

(defun startup/restart-signal (&optional arg)
  (interactive "p")
  (startup/kill-signal)
  (startup/start-signal arg))

(defun startup/select-signal ()
  (interactive)
  (unless (process-live-p startup/signal-process)
    (startup/start-signal))
  (+workspace-switch startup/signal-workspace t))

(map! :map +startup-applications-map
      :prefix-map ("s" . "signal")
      "r" #'startup/restart-signal
      "s" #'startup/select-signal
      "x" #'startup/kill-signal)
(after! which-key
  (which-key-add-keymap-based-replacements +startup-applications-map
    "s" "Signal"
    "s r" "Restart Signal"
    "s s" "Select Signal"
    "s x" "Kill Signal"))

;; (if (process-live-p startup/signal-process)
;;     (startup/restart-signal)
;;   (startup/start-signal))
