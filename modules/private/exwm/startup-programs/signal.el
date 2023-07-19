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
    (add-hook 'exwm-manage-finish-hook #'startup/manage-signal)
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
      (+workspace-new startup/signal-workspace ))))

(defun startup/select-signal ()
  (interactive)
  (unless (process-live-p startup/signal-process)
    (startup/start-signal))
  (+workspace-switch startup/signal-workspace t)
  (+workspace-switch-to-exwm-buffer-maybe))

(map! :leader
      :prefix +startup-prefix
      (:prefix ("s" . "Signal")
       :desc "Restart Signal" "r" #'startup/restart-signal
       :desc "Select Signal" "s" #'startup/select-signal
       :desc "Kill Signal" "x" #'startup/kill-signal))

;; (if (process-live-p startup/signal-process)
;;     (startup/restart-signal)
;;   (startup/start-signal))
