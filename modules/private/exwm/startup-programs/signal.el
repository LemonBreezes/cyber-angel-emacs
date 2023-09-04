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
                         (if (eq (user-uid) 0) "--no-sandbox" "")))
    (when arg (+workspace-switch startup/signal-workspace t))))

(defun startup/kill-signal (&optional arg)
  (interactive "p")
  (when (process-live-p startup/signal-process)
    (kill-process startup/signal-process))
  (when (and arg (+workspace-exists-p startup/signal-workspace))
    (when (string= startup/signal-workspace
                   (+workspace-current-name))
      (+workspace/other))
    (+workspace-delete startup/signal-workspace)))

(defun startup/restart-signal (&optional arg)
  (interactive "p")
  (startup/kill-signal)
  (startup/start-signal arg))

(defun startup/manage-signal ()
  (when (and (stringp exwm-class-name)
             (string-match-p "signal" exwm-class-name))
    (unless (string= (+workspace-current-name) startup/signal-workspace)
      (previous-buffer))
    (unless (+workspace-exists-p startup/signal-workspace)
      (+workspace-new startup/signal-workspace)
      (set-persp-parameter 'dont-save-to-file t (persp-get-by-name startup/signal-workspace)))))

(defun startup/select-signal ()
  (interactive)
  (unless (process-live-p startup/signal-process)
    (startup/start-signal))
  (+workspace-switch startup/signal-workspace t)
  (+workspace-switch-to-exwm-buffer-maybe))

(map! :map +startup-applications-map
      :prefix "s"
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
