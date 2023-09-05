;;; startup/teams.el -*- lexical-binding: t; -*-

(defvar startup/teams-workspace "Teams")

(defun startup/manage-teams ()
  (when (and (stringp exwm-class-name)
             (string-match-p "teams" exwm-class-name))
    (unless (string= (+workspace-current-name) startup/teams-workspace)
      (previous-buffer))))

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

(map! :map +startup-applications-map
      :prefix "t"
      "r" #'startup/restart-teams
      "s" #'startup/select-teams
      "x" #'startup/kill-teams)
(after! which-key
(which-key-add-keymap-based-replacements +startup-applications-map
  "t"   "Teams"
  "t r" "Restart Teams"
  "t s" "Select Teams"
  "t x" "Kill Teams"))
