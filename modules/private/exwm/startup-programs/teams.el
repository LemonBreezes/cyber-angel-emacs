;;; startup/teams.el -*- lexical-binding: t; -*-

(defvar startup/teams-workspace "Teams")

(defun startup/manage-teams ()
  (when (and (stringp exwm-class-name)
             (string-match-p "teams" exwm-class-name))
    (unless (string= (+workspace-current-name) startup/teams-workspace)
      (previous-buffer))
    (unless (+workspace-exists-p startup/teams-workspace)
      (+workspace-new startup/teams-workspace ))))

(defun startup/kill-teams (&optional arg)
  (interactive "p")
  (when (and arg (+workspace-exists-p startup/teams-workspace))
    (when (string= startup/teams-workspace
                   (+workspace-current-name))
      (+workspace/other))
    (+workspace-delete startup/teams-workspace))
  (shell-command-to-string "killall teams")
  (mapc (lambda (frame)
          (when (frame-live-p frame)
            (when (thread-last frame
                               (frame-first-window)
                               (window-buffer)
                               (buffer-name)
                               (string-match-p "Microsoft Teams"))
              (with-selected-frame frame
                (exwm-floating--exit)))))
   (visible-frame-list)))

(defun startup/restart-teams ()
  (interactive)
  (startup/kill-teams)
  ;; For some reason starting it with `start-process' doesn't work.
  (shell-command-to-string "teams --no-sandbox"))

(defun startup/select-teams ()
  (interactive)
  (unless (+workspace-exists-p startup/teams-workspace)
    (startup/restart-teams))
  (+workspace-switch startup/teams-workspace t))

(map! :leader
      :prefix +startup-prefix
      (:prefix ("t" . "Teams")
       :desc "Restart Teams" "r" #'startup/restart-teams
       :desc "Select Teams" "s" #'startup/select-teams
       :desc "Kill Teams" "x" #'startup/kill-teams))

(when (executable-find "teams")
  (add-hook 'exwm-manage-finish-hook #'startup/manage-teams))
