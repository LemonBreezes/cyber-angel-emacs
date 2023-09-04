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
      (+workspace-new startup/tiled-workspace)
      (set-persp-parameter 'dont-save-to-file t (persp-get-by-name startup/tiled-workspace)))))

(defun startup/select-tiled ()
  (interactive)
  (unless (process-live-p startup/tiled-process)
    (startup/start-tiled))
  (+workspace-switch startup/tiled-workspace t)
  (+workspace-switch-to-exwm-buffer-maybe))

(map! :map +startup-applications-map
      :prefix "T"
      "r" #'startup/restart-tiled
      "s" #'startup/select-tiled
      "x" #'startup/kill-tiled)
(which-key-add-keymap-based-replacements +startup-applications-map
  "T" "Tiled"
  "T r" "Restart Tiled"
  "T s" "Select Tiled"
  "T x" "Kill Tiled")
(add-hook 'exwm-manage-finish-hook #'startup/manage-tiled)

;; (if (process-live-p startup/tiled-process)
;;     (startup/restart-tiled)
;;   (startup/start-tiled))
