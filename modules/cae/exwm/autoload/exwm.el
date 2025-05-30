;;; cae/exwm/autoload/evil.el -*- lexical-binding: t; -*-

(defvar cae-exwm-vanilla-emacs-config-dir
  (concat doom-user-dir "vanilla-emacs-configs/"))
(defvar cae-exwm-vanilla-emacs--config-history nil)
(defvar cae-exwm-vanilla-doom-emacs-config-dir
  (concat doom-user-dir "profiles/"))
(defvar cae-exwm-vanilla-doom-emacs--config-history nil)

;;;###autoload
(defun cae-exwm-open-nested-emacs (arg)
  "Open a separate GUI instance of Emacs. If ARG is non-nil, debug init as
well."
  (interactive "P")
  (apply #'start-process "Emacs" nil "emacs"
         (when arg (list "--debug-init"))))

;;;###autoload
(defun cae-exwm-open-nested-vanilla-emacs (arg)
  "Open a separate GUI instance of Emacs with a vanilla config. If ARG is
non-nil, debug init as well."
  (interactive "P")
  (apply #'start-process "Emacs" nil "emacs" "-Q"
         "-l"
         (expand-file-name
          (completing-read "Load file: "
                           (seq-filter (lambda (f)
                                         (not (string-prefix-p "flycheck_" f)))
                                       (directory-files cae-exwm-vanilla-emacs-config-dir nil "^[^.]"))
                           nil t nil 'cae-exwm-vanilla-emacs--config-history)
          cae-exwm-vanilla-emacs-config-dir)
         (when arg (list "--debug-init"))))

;;;###autoload
(defun cae-exwm-open-nested-vanilla-doom-emacs (arg)
  "Open a separate GUI instance of Doom Emacs. If ARG is non-nil, debug init"
  (interactive "P")
  (let ((process-environment
         (cons
          (format "DOOMPROFILE=%s"
                  (file-name-base
                   (completing-read "Set DOOMPROFILE: "
                                    (seq-filter (lambda (f)
                                                  (not (string-prefix-p "flycheck_" f)))
                                                (directory-files cae-exwm-vanilla-doom-emacs-config-dir nil "^[^.]"))
                                    nil t nil 'cae-exwm-vanilla-doom-emacs--config-history)))
          process-environment)))
    (apply #'start-process "Emacs" nil "emacs"
           (when arg (list "--debug-init")))))

(defvar cae-exwm-backlight-device nil
  "Cached backlight device path.")

;;;###autoload
(defun cae-exwm-decrease-brightness ()
  "Decrease screen brightness using light."
  (interactive)
  (if-let ((device (cae-exwm--get-backlight-device)))
      (start-process "light" nil "light" "-U" "10")
    (message "No backlight device found")))

;;;###autoload
(defun cae-exwm-increase-brightness ()
  "Increase screen brightness using light."
  (interactive)
  (if-let ((device (cae-exwm--get-backlight-device)))
      (start-process "light" nil "light" "-A" "10")
    (message "No backlight device found")))

(defvar cae-exwm-keyboard-backlight-device nil
  "Cached keyboard backlight device path.")

(defun cae-exwm--get-keyboard-backlight-device ()
  "Get the keyboard backlight device path, with caching."
  (or cae-exwm-keyboard-backlight-device
      (setq cae-exwm-keyboard-backlight-device
            (car (directory-files "/sys/class/leds/" nil ".*kbd_backlight$")))))

;;;###autoload
(defun cae-exwm-decrease-keyboard-brightness ()
  "Decrease keyboard backlight brightness using light."
  (interactive)
  (if-let ((device (cae-exwm--get-keyboard-backlight-device)))
      (start-process "light" nil "light" "-s" (concat "sysfs/leds/" device) "-U" "34")
    (message "No keyboard backlight device found")))

;;;###autoload
(defun cae-exwm-increase-keyboard-brightness ()
  "Increase keyboard backlight brightness using light."
  (interactive)
  (if-let ((device (cae-exwm--get-keyboard-backlight-device)))
      (start-process "light" nil "light" "-s" (concat "sysfs/leds/" device) "-A" "34")
    (message "No keyboard backlight device found")))

;;;###autoload
(defun cae-exwm-workspace-switch-next ()
  "Switch to the next EXWM workspace.
Wraps around from the last workspace to the first."
  (interactive)
  (let* ((current-index exwm-workspace-current-index)
         (count (exwm-workspace--count))
         (next-index (mod (1+ current-index) count)))
    ;; Avoid switching if there's only one workspace or
    ;; if the next index is the same as the current (shouldn't happen with mod).
    (unless (= count 1)
      (exwm-workspace-switch next-index))))

;;;###autoload
(defun cae-exwm-workspace-switch-previous ()
  "Switch to the previous EXWM workspace.
Wraps around from the first workspace to the last."
  (interactive)
  (let* ((current-index exwm-workspace-current-index)
         (count (exwm-workspace--count))
         ;; `mod' handles negative numbers correctly for wrap-around.
         (prev-index (mod (1- current-index) count)))
    ;; Avoid switching if there's only one workspace.
    (unless (= count 1)
      (exwm-workspace-switch prev-index))))
