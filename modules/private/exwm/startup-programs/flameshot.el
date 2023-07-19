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

(map! :leader
      :prefix +startup-prefix
      (:prefix ("f" . "Flameshot")
       :desc "Restart Flameshot" "r" #'startup/restart-flameshot
       :desc "Kill Flameshot" "x" #'startup/kill-flameshot
       :desc "Take screenshot" "f" #'startup/flameshot-take-screenshot))

;; (if (process-live-p startup/flameshot-process)
;;     (startup/restart-flameshot)
;;   (startup/start-flameshot))
