;;; private/exwm/startup-programs/tiled.el -*- lexical-binding: t; -*-

(defvar startup/tiled-process nil)
(defvar startup/tiled-executable "~/src/Tiled-1.10.1_Linux_Qt-6_x86_64.AppImage")
(defvar startup/tiled-workspace "Tiled")

(defun startup/start-tiled (&optional arg)
  (when startup/tiled-executable
    (setq startup/tiled-process
          (start-process "tiled"
                         " *startup/tiled*"
                         startup/tiled-executable))
    (add-hook 'exwm-manage-finish-hook #'startup/manage-tiled)
    (when arg (+workspace-switch startup/tiled-workspace t))))

(defun startup/kill-tiled (&optional arg)
  (interactive "p")
  (when (process-live-p startup/tiled-process)
    (kill-process startup/tiled-process))
  (when (and arg (+workspace-exists-p startup/tiled-workspace))
    (when (string= startup/tiled-workspace
                   (+workspace-current-name))
      (+workspace/other))
    (+workspace-delete startup/tiled-workspace)))

(defun startup/restart-tiled (&optional arg)
  (interactive "p")
  (startup/kill-tiled)
  (startup/start-tiled arg))

(defun startup/manage-tiled ()
  (when (and (stringp exwm-class-name)
             (string-match-p "tiled" exwm-class-name))
    (unless (string= (+workspace-current-name) startup/tiled-workspace)
      (previous-buffer))
    (unless (+workspace-exists-p startup/tiled-workspace)
      (+workspace-new startup/tiled-workspace ))))

(defun startup/select-tiled ()
  (interactive)
  (unless (process-live-p startup/tiled-process)
    (startup/start-tiled))
  (+workspace-switch startup/tiled-workspace t)
  (+workspace-switch-to-exwm-buffer-maybe))

(map! :leader
      :prefix +startup-prefix
      (:prefix ("T" . "Tiled")
       :desc "Restart Tiled" "r" #'startup/restart-tiled
       :desc "Select Tiled" "s" #'startup/select-tiled
       :desc "Kill Tiled" "x" #'startup/kill-tiled))

;; (if (process-live-p startup/tiled-process)
;;     (startup/restart-tiled)
;;   (startup/start-tiled))
