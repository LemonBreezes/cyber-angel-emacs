;;; private/exwm/startup-programs/gimp.el -*- lexical-binding: t; -*-

(defvar startup/gimp-process nil)
(defvar startup/gimp-executable (executable-find "gimp"))
(defvar startup/gimp-workspace "GIMP")

(defun startup/start-gimp (&optional arg)
  (when startup/gimp-executable
    (setq startup/gimp-process
          (start-process "gimp"
                         " *startup/gimp*"
                         startup/gimp-executable))
    (add-hook 'exwm-manage-finish-hook #'startup/manage-gimp)
    (when arg (+workspace-switch startup/gimp-workspace t))))

(defun startup/kill-gimp (&optional arg)
  (interactive "p")
  (when (process-live-p startup/gimp-process)
    (kill-process startup/gimp-process))
  (when (and arg (+workspace-exists-p startup/gimp-workspace))
    (when (string= startup/gimp-workspace
                   (+workspace-current-name))
      (+workspace/other))
    (+workspace-delete startup/gimp-workspace)))

(defun startup/restart-gimp (&optional arg)
  (interactive "p")
  (startup/kill-gimp)
  (startup/start-gimp arg))

(defun startup/manage-gimp ()
  (when (and (stringp exwm-class-name)
             (string-match-p "gimp" exwm-class-name))
    (setq-local exwm-input--simulation-keys nil)
    (unless (string= (+workspace-current-name) startup/gimp-workspace)
      (previous-buffer))
    (unless (+workspace-exists-p startup/gimp-workspace)
      (+workspace-new startup/gimp-workspace ))))

(defun startup/select-gimp ()
  (interactive)
  (unless (process-live-p startup/gimp-process)
    (startup/start-gimp))
  (+workspace-switch startup/gimp-workspace t)
  (+workspace-switch-to-exwm-buffer-maybe))

(map! :leader
      :prefix +startup-prefix
      (:prefix ("g" . "GIMP")
       :desc "Restart GIMP" "r" #'startup/restart-gimp
       :desc "Select GIMP" "s" #'startup/select-gimp
       :desc "Kill GIMP" "x" #'startup/kill-gimp))

;; (if (process-live-p startup/gimp-process)
;;     (startup/restart-gimp)
;;   (startup/start-gimp))
