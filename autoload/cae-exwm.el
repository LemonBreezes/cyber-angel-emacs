;;; autoload/cae-exwm.el -*- lexical-binding: t; -*-


(defvar cae-exwm--redshift-process nil
  "Process object for the running redshift instance.")

;;;###autoload
(defun cae-exwm-lock-screen ()
  "Lock the screen using i3lock."
  (interactive)
  (let ((process (start-process "i3lock" nil "i3lock" "-n" "-c" "000000")))
    (set-process-sentinel process (lambda (process event)
                                    (when (string-prefix-p "finished" event)
                                      (message "Screen unlocked"))))))

;;;###autoload
(defun cae-exwm-run-virtualbox ()
  "Run VirtualBox with proper permissions."
  (interactive)
  (start-process-shell-command
   "virtualbox" "*VIRTUAL*"
   (format "sudo XDG_RUNTIME_DIR=/run/user/%d virtualbox"
           (user-uid))))

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
                           "6500:3500"
                           "-b" "1.0:0.8"))
      (message "Redshift turned on for %s" (cdr (assq 'name cae-location-data))))))
