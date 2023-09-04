;;; startup/mpv.el -*- lexical-binding: t; -*-

(defvar startup/mpv-workspace "mpv")

(defun startup/manage-mpv ()
  (when (and (stringp exwm-class-name)
             (string-match-p "mpv" exwm-class-name))
    (unless (string= (+workspace-current-name) startup/mpv-workspace)
      (previous-buffer))
    (unless (+workspace-exists-p startup/mpv-workspace)
      (+workspace-new startup/mpv-workspace))))

(defun startup/mpv-setup-workspace (persp _)
  (when (string= persp-name startup/mpv-workspace)
    (set-persp-parameter 'dont-save-to-file t persp)))

(defun startup/mpv-kill-mpv ()
  (interactive)
  (+workspace-delete startup/mpv-workspace))

(add-hook 'exwm-manage-finish-hook #'startup/manage-mpv)
(advice-add #'empv-exit :after #'startup/mpv-kill-mpv)
(add-hook 'persp-created-functions #'startup/mpv-setup-workspace)
