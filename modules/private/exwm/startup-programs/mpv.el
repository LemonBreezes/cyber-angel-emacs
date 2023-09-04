;;; startup/mpv.el -*- lexical-binding: t; -*-

(defvar startup/mpv-workspace "mpv")

(defun startup/manage-mpv ()
  (when (and (stringp exwm-class-name)
             (string-match-p "mpv" exwm-class-name))
    (unless (string= (+workspace-current-name) startup/mpv-workspace)
      (previous-buffer))
    (unless (+workspace-exists-p startup/mpv-workspace)
      (+workspace-new startup/mpv-workspace)
      (set-persp-parameter 'dont-save-to-file t (persp-get-by-name startup/mpv-workspace)))))
(add-hook 'exwm-manage-finish-hook #'startup/manage-mpv)
