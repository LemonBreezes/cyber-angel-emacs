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

;;;###autoload
(defun cae-exwm-decrease-brightness ()
  "Decrease screen brightness using xbacklight."
  (interactive)
  (start-process "xbacklight" nil "xbacklight" "-dec" "10"))

;;;###autoload
(defun cae-exwm-increase-brightness ()
  "Increase screen brightness using xbacklight."
  (interactive)
  (start-process "xbacklight" nil "xbacklight" "-inc" "10"))
