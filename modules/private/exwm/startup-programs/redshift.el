;;; startup/redshift.el -*- lexical-binding: t; -*-

(defvar startup/redshift-process nil)
(defvar startup/redshift-timer nil)

(unless (executable-find "redshift")
  (error "redshift is missing from your PATH."))

(defun startup/start-redshift (&optional arg)
  (when (and (bound-and-true-p calendar-latitude)
             (bound-and-true-p calendar-longitude))
    (advice-add #'+workspace-switch :after #'startup/restart-redshift)
    (setq startup/redshift-process
          (apply #'start-process
                 "redshift" " *startup/redshift*" "redshift"
                 "-l" (concat (number-to-string calendar-latitude)
                              ":"
                              (number-to-string calendar-longitude))
                 (pcase (modus-themes--current-theme)
                   ((or modus-operandi modus-operandi-tinted
                        modus-operandi-tritanopia modus-operandi-deuteranopia)
                    `("-P" "-O" "5200" "-b"
                      ;; Set monitor brightness lower in certain applications.
                      ,(pcase (+workspace-current-name)
                         ("Chromium" "1.0")
                         ("Teams" "0.6")
                         ("Discord" "1.0")
                         (_ "1.0"))
                      "-g" "1.0"))
                   ((or modus-vivendi modus-vivendi-tinted
                        modus-vivendi-tritanopia modus-vivendi-deuteranopia)
                    '("-P" "-O" "6300" "-b" "1.0" "-g" "1.0")))))))

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

(map! :leader
      :prefix +startup-prefix
      (:prefix ("r" . "Redshift")
       :desc "restart Redshift" "r" #'startup/restart-redshift
       :desc "Kill Redshift" "x" #'startup/kill-redshift))

(add-hook 'circadian-after-load-theme-hook #'startup/restart-redshift)
;;(advice-add #'+workspace-switch :after #'startup/restart-redshift)

(if (process-live-p startup/redshift-process)
    (startup/restart-redshift)
  (startup/start-redshift))
