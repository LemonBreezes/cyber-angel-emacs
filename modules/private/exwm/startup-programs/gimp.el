;;; private/exwm/startup-programs/gimp.el -*- lexical-binding: t; -*-

(defvar startup/gimp-process nil)
(defvar startup/gimp-executable (executable-find "gimp"))

(defun startup/start-gimp (&optional arg)
  (when startup/gimp-executable
    (setq startup/gimp-process
          (start-process "gimp"
                         " *startup/gimp*"
                         startup/gimp-executable))))

(defun startup/kill-gimp (&optional arg)
  (interactive "p")
  (when (process-live-p startup/gimp-process)
    (kill-process startup/gimp-process)))

(defun startup/restart-gimp (&optional arg)
  (interactive "p")
  (startup/kill-gimp)
  (startup/start-gimp arg))

(defun startup/select-gimp ()
  (interactive)
  (unless (process-live-p startup/gimp-process)
    (startup/start-gimp))
  (+workspace-switch startup/gimp-workspace t))

(map! :map +startup-applications-map
      :prefix "g"
      "r" #'startup/restart-gimp
      "s" #'startup/select-gimp
      "x" #'startup/kill-gimp)
(after! which-key
  (which-key-add-keymap-based-replacements +startup-applications-map
    "g" "GIMP"
    "g r" "Restart GIMP"
    "g s" "Select GIMP"
    "g x" "Kill GIMP"))
(add-hook 'exwm-manage-finish-hook #'startup/manage-gimp)

;; (if (process-live-p startup/gimp-process)
;;     (startup/restart-gimp)
;;   (startup/start-gimp))
