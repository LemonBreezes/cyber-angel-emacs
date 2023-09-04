;;; startup/pavucontrol.el -*- lexical-binding: t; -*-

(defvar startup/pavucontrol-process nil)
(defvar startup/pavucontrol-executable (executable-find "pavucontrol"))
(defvar startup/pavucontrol-workspace "Pavucontrol")

(defun startup/start-pavucontrol (&optional arg)
  (when startup/pavucontrol-executable
    (setq startup/pavucontrol-process
          (start-process "pavucontrol"
                         " *startup/pavucontrol*"
                         startup/pavucontrol-executable))
    (when arg (+workspace-switch startup/pavucontrol-workspace t)
          (set-persp-parameter 'dont-save-to-file t
                               (persp-get-by-name startup/pavucontrol-workspace)))))

(defun startup/kill-pavucontrol (&optional arg)
  (interactive "p")
  (when (process-live-p startup/pavucontrol-process)
    (kill-process startup/pavucontrol-process))
  (when (and arg (+workspace-exists-p startup/pavucontrol-workspace))
    (when (string= startup/pavucontrol-workspace
                   (+workspace-current-name))
      (+workspace/other))
    (+workspace-delete startup/pavucontrol-workspace)))

(defun startup/restart-pavucontrol (&optional arg)
  (interactive "p")
  (startup/kill-pavucontrol)
  (startup/start-pavucontrol arg))

(defun startup/manage-pavucontrol ()
  (when (and (stringp exwm-class-name)
             (string-match-p "pavucontrol" exwm-class-name))
    (unless (string= (+workspace-current-name) startup/pavucontrol-workspace)
      (previous-buffer))
    (unless (+workspace-exists-p startup/pavucontrol-workspace)
      (+workspace-new startup/pavucontrol-workspace)
      (set-persp-parameter 'dont-save-to-file t (persp-get-by-name startup/pavucontrol-workspace)))))

(defun startup/select-pavucontrol ()
  (interactive)
  (unless (and (process-live-p startup/pavucontrol-process)
               (cl-find-if
                (lambda (buf)
                  (when-let
                      ((class (buffer-local-value 'exwm-class-name buf)))
                    (string-match-p "pavucontrol" class)))
                (doom-buffer-list)))
    (startup/restart-pavucontrol))
  (+workspace-switch startup/pavucontrol-workspace t)
  (set-persp-parameter 'dont-save-to-file t
                       (persp-get-by-name startup/pavucontrol-workspace))
  (+workspace-switch-to-exwm-buffer-maybe))

(map! :map +startup-applications-map
      :prefix "p"
      "r" #'startup/restart-pavucontrol
      "s" #'startup/select-pavucontrol
      "x" #'startup/kill-pavucontrol)
(after! which-key
  (which-key-add-keymap-based-replacements +startup-applications-map
    "p"   "Pavucontrol"
    "p r" "Restart Pavucontrol"
    "p s" "Select Pavucontrol"
    "p x" "Kill Pavucontrol"))
    (add-hook 'exwm-manage-finish-hook #'startup/manage-pavucontrol)
