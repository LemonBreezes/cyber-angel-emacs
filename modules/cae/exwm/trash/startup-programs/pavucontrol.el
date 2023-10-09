;;; startup/pavucontrol.el -*- lexical-binding: t; -*-

(defvar startup/pavucontrol-process nil)
(defvar startup/pavucontrol-executable (executable-find "pavucontrol"))
(defvar startup/pavucontrol-workspace "Pavucontrol")

(defun startup/start-pavucontrol (&optional arg)
  (when startup/pavucontrol-executable
    (setq startup/pavucontrol-process
          (start-process "pavucontrol"
                         " *startup/pavucontrol*"
                         startup/pavucontrol-executable))))

(defun startup/kill-pavucontrol (&optional arg)
  (interactive "p")
  (when (process-live-p startup/pavucontrol-process)
    (kill-process startup/pavucontrol-process)))

(defun startup/restart-pavucontrol (&optional arg)
  (interactive "p")
  (startup/kill-pavucontrol)
  (startup/start-pavucontrol arg))

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
  (+workspace-switch startup/pavucontrol-workspace t))

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
