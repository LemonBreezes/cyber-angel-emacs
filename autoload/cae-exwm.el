;;; autoload/cae-exwm.el -*- lexical-binding: t; -*-


(defvar cae-exwm--redshift-process nil
  "Process object for the running redshift instance.")

;;;###autoload
(defun cae-exwm-toggle-redshift ()
  "Toggle redshift on/off using location data from cae-location-data."
  (interactive)
  (if (and cae-exwm--redshift-process
           (process-live-p cae-exwm--redshift-process))
      (progn
        ;; Kill the existing process
        (interrupt-process cae-exwm--redshift-process)
        (delete-process cae-exwm--redshift-process)
        (setq cae-exwm--redshift-process nil)
        ;; Reset screen temperature
        (start-process "redshift-reset" nil "redshift" "-x")
        (message "Redshift turned off"))
    (let* ((lat (cdr (assq 'latitude cae-location-data)))
           (lon (cdr (assq 'longitude cae-location-data)))
           (lat-str (number-to-string lat))
           (lon-str (number-to-string lon)))
      ;; Start new redshift process and store the process object
      (setq cae-exwm--redshift-process
            (start-process "redshift" nil "redshift" "-l"
                           (concat lat-str ":" lon-str)
                           "6500:4000"))
      (message "Redshift turned on for %s" (cdr (assq 'name cae-location-data))))))
