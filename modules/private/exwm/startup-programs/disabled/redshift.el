;;; startup/redshift.el -*- lexical-binding: t; -*-

(defvar startup/redshift-process nil)
(defvar startup/redshift-timer nil)

(defun startup/start-redshift (&optional arg)
  (when (and (bound-and-true-p calendar-latitude)
             (bound-and-true-p calendar-longitude)
             (executable-find "redshift"))
    (advice-add #'+workspace-switch :after #'startup/restart-redshift)
    (setq startup/redshift-process
          (apply #'start-process
                 "redshift" " *startup/redshift*" "redshift"
                 "-l" (concat (number-to-string calendar-latitude)
                              ":"
                              (number-to-string calendar-longitude))
                 (if (cae-night-time-p)
                     `("-P" "-O" "3300" "-b"
                       ;; Set monitor brightness lower in certain applications.
                       ,(pcase (+workspace-current-name)
                          ("Teams" "1.0")
                          ("Discord" "1.0")
                          (_ "1.0"))
                       "-g" "1.0")
                   '("-P" "-O" "6300" "-b" "1.0" "-g" "1.0"))))))

(defun startup/kill-redshift (&optional arg)
  (interactive "p")
  (when (process-live-p startup/redshift-process)
    (kill-process startup/redshift-process))
  (when arg
    (start-process
     "redshift" " *startup/redshift*" "redshift"
     "-P" "-b" "1.0" "-g" "1.0" "-x"))
  (advice-remove #'+workspace-switch #'startup/restart-redshift))

(defun startup/restart-redshift (&optional arg &rest _)
  (interactive "p")
  (startup/kill-redshift)
  (startup/start-redshift arg))

(map! :map +startup-applications-map
      :prefix "r"
      "r" #'startup/restart-redshift
      "x" #'startup/kill-redshift)
(after! which-key
  (which-key-add-keymap-based-replacements +startup-applications-map
    "r" "Redshift"
    "rr" "Restart Redshift"
    "rx" "Kill Redshift"))


(add-hook 'doom-load-theme-hook #'startup/restart-redshift)
;;(advice-add #'+workspace-switch :after #'startup/restart-redshift)

;;(if (process-live-p startup/redshift-process)
;;    (startup/restart-redshift)
;;  (startup/start-redshift))
