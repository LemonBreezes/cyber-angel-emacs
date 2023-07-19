;;; startup/chromium.el -*- lexical-binding: t; -*-

(defvar startup/chromium-process nil)
(defvar startup/chromium-executable (or (executable-find "chromium-bin")
                                        (executable-find "chromium")))
(defvar startup/chromium-workspace "Chromium")

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

(map! :leader
      :prefix +startup-prefix
      (:prefix ("c" . "Chromium")
       :desc "Restart Chromium" "r" #'startup/restart-chromium
       :desc "Select Chromium" "s" #'startup/select-chromium
       :desc "Kill Chromium" "x" #'startup/kill-chromium))

(when (modulep! :private exwm)
  (defadvice! +exwm--firefox-bookmark-handler-a (orig-fn &rest args)
    (+workspace-switch "Chromium" t)
    (set-persp-parameter 'dont-save-to-file t (persp-get-by-name startup/chromium-workspace))
    (apply orig-fn args)))

(if (process-live-p startup/chromium-process)
    (startup/restart-chromium)
  (startup/start-chromium))
