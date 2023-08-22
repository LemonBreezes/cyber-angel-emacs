;;; startup/chromium.el -*- lexical-binding: t; -*-

(defvar startup/chromium-process nil)
(defvar startup/chromium-executable (or (executable-find "chromium-bin")
                        (executable-find "chromium")
                        (executable-find "google-chrome-unstable")))
(defvar startup/chromium-workspace (if (or (executable-find "chromium-bin")
                           (executable-find "chromium"))
                       "Chromium"
                     "Chrome"))

(defun startup/start-chromium (&optional arg)
  (when startup/chromium-executable
    (setq startup/chromium-process
          (start-process "chromium"
                         " *startup/chromium*"
                         startup/chromium-executable
                         (if (eq (user-uid) 0) "--no-sandbox" "")))
    (add-hook 'exwm-manage-finish-hook #'startup/manage-chromium)
    (when arg (+workspace-switch startup/chromium-workspace t)
          (set-persp-parameter 'dont-save-to-file t
                               (persp-get-by-name startup/chromium-workspace)))))

(defun startup/kill-chromium (&optional arg)
  (interactive "p")
  (when (process-live-p startup/chromium-process)
    (kill-process startup/chromium-process))
  (when (and arg (+workspace-exists-p startup/chromium-workspace))
    (when (string= startup/chromium-workspace
                   (+workspace-current-name))
      (+workspace/other))
    (+workspace-delete startup/chromium-workspace)))

(defun startup/restart-chromium (&optional arg)
  (interactive "p")
  (startup/kill-chromium)
  (startup/start-chromium arg))

(defun startup/manage-chromium ()
  (when (and (stringp exwm-class-name)
             (string-match-p "chromium" exwm-class-name))
    (unless (string= (+workspace-current-name) startup/chromium-workspace)
      (previous-buffer))
    (unless (+workspace-exists-p startup/chromium-workspace)
      (+workspace-new startup/chromium-workspace))))

(defun startup/select-chromium ()
  (interactive)
  (unless (and (process-live-p startup/chromium-process)
               (cl-find-if
                (lambda (buf)
                  (when-let
                      ((class (buffer-local-value 'exwm-class-name buf)))
                    (string-match-p "chromium" class)))
                (doom-buffer-list)))
    (startup/restart-chromium))
  (+workspace-switch startup/chromium-workspace t)
  (set-persp-parameter 'dont-save-to-file t
                       (persp-get-by-name startup/chromium-workspace))
  (+workspace-switch-to-exwm-buffer-maybe))

(map! :map +startup-applications-map
      :prefix "c"
      "r" #'startup/restart-chromium
      "s" #'startup/select-chromium
      "x" #'startup/kill-chromium)
(which-key-add-keymap-based-replacements +startup-applications-map
  "c" "Chromium"
  "c r" "Restart Chromium"
  "c s" "Select Chromium"
  "c x" "Kill Chromium")

(if (process-live-p startup/chromium-process)
    (startup/restart-chromium)
  (unless (getenv "SSH_TTY")
    (startup/start-chromium)))

(defadvice! +startup/chromium-browse-url-generic-a (&rest _)
  :before #'browse-url-generic
  (when (equal startup/chromium-executable browse-url-generic-program)
    (+workspace-switch startup/chromium-workspace t)
    (+workspace/display)))
