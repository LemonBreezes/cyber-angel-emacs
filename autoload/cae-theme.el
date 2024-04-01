;;; autoload/cae-theme.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-dark-theme-p ()
  (eq (frame-parameter nil 'background-mode) 'dark))

;;;###autoload
(defun cae-night-time-p ()
  (when-let ((now (reverse (cl-subseq (decode-time) 0 3)))
             (sunset (or (doom-store-get 'circadian-sunset)
                         (require 'circadian nil t)
                         (circadian-sunset)))
             (sunrise (or (doom-store-get 'circadian-sunrise)
                          (require 'circadian nil t)
                          (circadian-sunrise))))
    (doom-store-put 'circadian-sunset sunset)
    (doom-store-put 'circadian-sunrise sunrise)
    (or (and (>= (cl-first now) (cl-first sunset)))
        (and (< (cl-first now) (cl-first sunrise)))
        (and (= (cl-first now) (cl-first sunset))
             (>= (cl-second now) (cl-second sunset)))
        (and (= (cl-first now) (cl-first sunrise))
             (< (cl-second now) (cl-second sunrise))))))

;;;###autoload
(defun cae-theme-export-using-pywal ()
  (interactive)
  (when (and (executable-find "python")
             (executable-find "wal")
             (require 'theme-magic nil t)
             (require 'ewal nil t))
    (let (;; If we're in a pyenv directory, we might accidentally run the virtual
          ;; version of Python instead of the user's root version. To fix this, we
          ;; temporarily change to the user's dir.
          (default-directory "~/"))
      (let ((theme-magic-colors (theme-magic--auto-extract-16-colors)))
        ;; Set the environment variable WAL_COLORN for N the color number to the
        ;; color value.
        (cl-loop for color in theme-magic-colors
                 for i upfrom 1
                 do (setenv (format "WAL_COLOR%s" i) color))
        (set-process-sentinel
         (apply #'start-process
                "cae-theme-magic"
                nil
                "python"
                theme-magic--pywal-python-script
                theme-magic-colors)
         (lambda (_proc event)
           (when (string= event "finished\n")
             (ewal-load-colors)
             (when (executable-find "polybar-msg")
               (start-process "restart polybar" nil "polybar-msg" "cmd" "restart"))
             (when (executable-find "dunst")
               ;; This assumes you are running `dunst' as a systemd service.
               (start-process "kill dunst" nil "killall" "dunst")))))))))

;;;###autoload
(defun cae-theme-refresh-latex-images-previews-h ()
  (dolist (buffer (doom-buffers-in-mode 'org-mode (buffer-list)))
    (with-current-buffer buffer
      (+org--toggle-inline-images-in-subtree (point-min) (point-max) 'refresh)
      (unless (eq org-preview-latex-default-process 'dvisvgm)
        (org-clear-latex-preview (point-min) (point-max))
        (ignore-errors (org--latex-preview-region (point-min) (point-max)))))))

;;;###autoload
(defun cae-theme-ring-bell-function ()
  (let ((buf (current-buffer))
        (cookie (face-remap-add-relative 'mode-line-active
                                         'cae-modeline-bell-face)))
    (force-mode-line-update)
    (run-with-timer 0.1 nil
                    (lambda ()
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (face-remap-remove-relative cookie)
                          (force-mode-line-update)))))))
