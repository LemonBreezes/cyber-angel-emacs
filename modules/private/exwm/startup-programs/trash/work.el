;;; startup/work.el -*- lexical-binding: t; -*-

;; Just a command to do work stuff for my job.
(defun startup/restart-work ()
  (interactive)
  (startup/kill-work)
  (startup/restart-teams)
  (startup/restart-virtualbox)
  (advice-add #'projectile-find-file :around #'ignore)
  (unwind-protect (projectile-switch-project-by-name "~/src/atlas")
    (advice-remove #'projectile-find-file #'ignore))
  (magit-status))

(defun startup/kill-work (&optional arg)
  (interactive "p")
  (startup/kill-teams arg)
  (startup/kill-virtualbox arg)
  (when (and arg (+workspace-exists-p "atlas"))
    (+workspace-delete "atlas")))

(defun startup/select-work ()
  (interactive)
  (unless (+workspace-exists-p "atlas")
    (startup/restart-work))
  (+workspace-switch "atlas"))

(map! :leader
      :prefix +startup-prefix
      (:prefix ("w" . "work")
       :desc "Restart work" "r" #'startup/restart-work
       :desc "Select work" "s" #'startup/select-work
       :desc "Kill work" "x" #'startup/kill-work))
