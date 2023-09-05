;;; startup/mpv.el -*- lexical-binding: t; -*-

(defvar startup/mpv-workspace "mpv")

(defun startup/mpv-kill-mpv ()
  (interactive)
  (when (+workspace-exists-p startup/mpv-workspace)
    (+workspace-delete startup/mpv-workspace)))

(advice-add #'empv-exit :after #'startup/mpv-kill-mpv)
