;;; startup/discord.el -*- lexical-binding: t; -*-

(defvar startup/discord-process nil)
(defvar startup/discord-executable (executable-find "discord"))
(defvar startup/discord-workspace "Discord")
(defvar startup/discord--timer nil)

(define-minor-mode startup/discord-mode
  "Minor mode for Discord."
  :init-value nil
  :keymap (make-sparse-keymap)
  :global nil)

(map! :map startup/discord-mode-map
      :n "K" (cmd! ()
                   (exwm-input--fake-key 'C-k)
                   (exwm-evil-insert)))

(defun startup/start-discord (&optional arg)
  (when startup/discord-executable
    (setq startup/discord-process
          (start-process "discord"
                         " *startup/discord*"
                         startup/discord-executable
                         (if (eq (user-uid) 0) "--no-sandbox" "")))
    (when arg (+workspace-switch startup/discord-workspace t)
          (set-persp-parameter 'dont-save-to-file t
                               (persp-get-by-name startup/discord-workspace)))))

(defun startup/kill-discord (&optional arg)
  (interactive "p")
  (when (process-live-p startup/discord-process)
    (kill-process startup/discord-process))
  (when (and arg (+workspace-exists-p startup/discord-workspace))
    (when (string= startup/discord-workspace
                   (+workspace-current-name))
      (+workspace/other))
    (+workspace-delete startup/discord-workspace)))

(defun startup/restart-discord (&optional arg)
  (interactive "p")
  (startup/kill-discord)
  (startup/start-discord arg))

(defun startup/manage-discord ()
  (when (and (stringp exwm-class-name)
             (string-match-p "discord" exwm-class-name))
    (startup/discord-mode +1)
    (unless (+workspace-exists-p startup/discord-workspace)
      (+workspace-new startup/discord-workspace)
      (set-persp-parameter 'dont-save-to-file t (persp-get-by-name startup/discord-workspace))))
  (when (persp-get-by-name startup/discord-workspace)
    (persp-add-buffer
     (cl-remove-if-not
      (lambda (buf)
        (string= "discord" (buffer-local-value 'exwm-class-name buf)))
      (buffer-list))
     (persp-get-by-name startup/discord-workspace))))

(defun startup/select-discord ()
  (interactive)
  (unless (process-live-p startup/discord-process)
    (startup/restart-discord))
  (+workspace-switch startup/discord-workspace t)
  (set-persp-parameter 'dont-save-to-file t
                       (persp-get-by-name startup/discord-workspace))
  (setq startup/discord--timer
        (run-at-time 1 0.05
                     (lambda ()
                       (if (string= "discord" exwm-class-name)
                           (cancel-timer startup/discord--timer)
                         (+workspace-switch-to-exwm-buffer-maybe)))))
  (defadvice! tmp/cancel-discord-timer-a (&rest _)
    :before #'+workspace-switch
    (cancel-timer startup/discord--timer)
    (advice-remove #'+workspace/other #'tmp/cancel-discord-timer-a)))

;; HACK Prevent an error that happens when there is no Discord process.
(defadvice! startup/discord-elcord-a ()
  :before-while #'elcord--start-idle
  (process-live-p startup/discord-process))

(map! :map +startup-applications-map
      :prefix "d"
      "r" #'startup/restart-discord
      "s" #'startup/select-discord
      "x" #'startup/kill-discord)
(after! which-key
  (which-key-add-keymap-based-replacements +startup-applications-map
    "d" "Discord"
    "d r" "Restart Discord"
    "d s" "Select Discord"
    "d x" "Kill Discord"))

(if (process-live-p startup/discord-process)
    (startup/restart-discord)
  (unless (getenv "SSH_TTY")
    (startup/start-discord)))
(add-hook 'exwm-manage-finish-hook #'startup/manage-discord)
