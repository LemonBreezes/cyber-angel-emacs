;;; startup/teams.el -*- lexical-binding: t; -*-

(defvar startup/teams-workspace "Teams")
(defvar startup/teams-executable (or (executable-find "teams-for-linux")
                        (executable-find "teams")))
(defvar startup/teams-buffer (get-buffer-create "*startup/teams*"))

(defun startup/kill-teams (&optional arg)
  (interactive "p")
  (when (and arg (+workspace-exists-p startup/teams-workspace))
    (when (string= startup/teams-workspace
                   (+workspace-current-name))
      (+workspace/other))
    (+workspace-kill startup/teams-workspace))
  (shell-command-to-string (concat "killall " (file-name-nondirectory startup/teams-executable)))
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
  (start-process "Teams" (get-buffer-create startup/teams-buffer)
                 startup/teams-executable
                 (if (eq (user-uid) 0) " --no-sandbox" " ")))

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
