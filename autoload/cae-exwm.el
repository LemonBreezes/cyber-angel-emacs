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
   "virtualbox" nil
   (format "sudo XDG_RUNTIME_DIR=/run/user/%d VirtualBox"
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
    (require 'calendar)
    (let* ((lat calendar-latitude)
           (lon calendar-longitude)
           (lat-str (number-to-string lat))
           (lon-str (number-to-string lon)))
      ;; Start new Redshift process and store the process object
      (setq cae-exwm--redshift-process
            (start-process "redshift" nil "redshift" "-l"
                           (concat lat-str ":" lon-str)
                           "6500:3500"
                           "-b" "1.0:0.8"))
      (message "Redshift turned on for %s" calendar-location-name))))

;;;###autoload
(defun cae-exwm-create-black-screen ()
  "Create a full-screen buffer with a black background.
Restores the previous window configuration on exit ('q')."
  (interactive)
  ;; Save the current window configuration
  (let ((saved-wconf (current-window-configuration))
        (black-buffer (get-buffer-create "*Black Screen*")))
    (when (featurep 'tab-bar)
      (let ((tab-bar-show nil))
        (tab-bar--update-tab-bar-lines t)))
    (switch-to-buffer black-buffer)
    (special-mode)
    (erase-buffer)
    ;; Set background to black for the current window configuration
    (face-remap-add-relative 'default :background "black")
    (delete-other-windows)
    (setq cursor-type nil)
    (message "Black screen created. Press 'q' to exit.")

    ;; Define the exit function
    (fset 'cae-exwm--exit-black-screen
          `(lambda ()
             (interactive)
             (set-window-configuration ,saved-wconf)
             (kill-buffer ,black-buffer)
             (when (featurep 'tab-bar)
               (tab-bar--update-tab-bar-lines t))))

    ;; Set up local keymap
    (use-local-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "q") #'cae-exwm--exit-black-screen)
       (define-key map [remap self-insert-command] #'ignore)
       (when (featurep 'evil)
         (define-key (evil-get-auxiliary-keymap map 'normal t)
                     (kbd "q") #'cae-exwm--exit-black-screen))
       map))
    (ignore-errors (evil-emacs-state)))
  (hide-mode-line-mode +1))
