;;; autoload/cae-theme.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-dark-theme-p ()
  (eq (frame-parameter nil 'background-mode) 'dark))

;;;###autoload
(defun cae-night-time-p ()
  (when-let* ((_ (and (bound-and-true-p calendar-latitude)
                      (bound-and-true-p calendar-longitude)))
              (now (reverse (cl-subseq (decode-time) 0 3)))
              (sunset (or (doom-store-get 'circadian-sunset)
                          (and (require 'circadian nil t)
                               (circadian-sunset))))
              (sunrise (or (doom-store-get 'circadian-sunrise)
                           (and (require 'circadian nil t)
                                (circadian-sunrise)))))
    (doom-store-put 'circadian-sunset sunset)
    (doom-store-put 'circadian-sunrise sunrise)
    (or (and (>= (cl-first now) (cl-first sunset)))
        (and (< (cl-first now) (cl-first sunrise)))
        (and (= (cl-first now) (cl-first sunset))
             (>= (cl-second now) (cl-second sunset)))
        (and (= (cl-first now) (cl-first sunrise))
             (< (cl-second now) (cl-second sunrise))))))

;;;###autoload
(defun cae-theme-refresh-latex-images-previews-h ()
  (when (autoloadp (symbol-function #'+org-get-todo-keywords-for))
    (autoload-do-load (symbol-function #'+org-get-todo-keywords-for)))
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

;;;###autoload
(defun cae-theme-toggle ()
  (interactive)
  (let ((current-theme (car custom-enabled-themes))
        (themes (custom-available-themes)))
    (dolist (theme (cdr custom-enabled-themes))
      (disable-theme theme))
    (cond ((memq current-theme `(,cae-day-theme ,cae-night-theme))
           (if (cae-dark-theme-p)
               (load-theme cae-day-theme t)
             (load-theme cae-night-theme t)))
          ((memq current-theme `(,cae-modus-day-theme ,cae-modus-night-theme))
           (modus-themes-toggle))
          ((memq current-theme `(,cae-ef-day-theme ,cae-ef-night-theme))
           (ef-themes-toggle))
          ;; Light/Dark theme pairs
          ((and (string-suffix-p "-light" (symbol-name current-theme))
                (memq (intern (concat (substring (symbol-name current-theme) 0 -6) "-dark"))
                      themes))
           (load-theme (intern (concat (substring (symbol-name current-theme) 0 -6) "-dark")) t))
          ((and (string-suffix-p "-dark" (symbol-name current-theme))
                (memq (intern (concat (substring (symbol-name current-theme) 0 -5) "-light"))
                      themes))
           (load-theme (intern (concat (substring (symbol-name current-theme) 0 -5) "-light")) t))
          ;; Night/Day theme pairs
          ((and (string-suffix-p "-day" (symbol-name current-theme))
                (memq (intern (concat (substring (symbol-name current-theme) 0 -4) "-night"))
                      themes))
           (load-theme (intern (concat (substring (symbol-name current-theme) 0 -4) "-night")) t))
          ((and (string-suffix-p "-night" (symbol-name current-theme))
                (memq (intern (concat (substring (symbol-name current-theme) 0 -6) "-day"))
                      themes))
           (load-theme (intern (concat (substring (symbol-name current-theme) 0 -6) "-day")) t))
          (t (load-theme current-theme t)))
    (dolist (theme (cdr custom-enabled-themes))
      (disable-theme theme))))

;;;###autoload
(defun cae-theme--get-circadian-config ()
  "Return the appropriate theme list for `circadian-themes'.
Uses sunrise/sunset if location is valid, otherwise fixed times."
  (if (and calendar-latitude calendar-longitude
           (numberp calendar-latitude)
           (numberp calendar-longitude)
           (not (= calendar-latitude 0))
           (not (= calendar-longitude 0))
           (not cae-circadian-used-fixed-times))
      (progn
        (message "Theme: Using sunrise/sunset for theme switching.")
        `((:sunrise . ,cae-day-theme)
          (:sunset . ,cae-night-theme)))
    (progn
      (when (and cae-geolocation-verbose
                 (not cae-circadian-used-fixed-times))
        (message "Theme: Geolocation not ready or invalid coordinates (%s, %s), using fixed times (%s/%s) for theme switching."
                 calendar-latitude
                 calendar-longitude
                 cae-circadian-fixed-day-time cae-circadian-fixed-night-time))
      `((,cae-circadian-fixed-day-time . ,cae-day-theme)
        (,cae-circadian-fixed-night-time . ,cae-night-theme)))))

;;;###autoload
(defun cae-theme--configure-circadian ()
  "Configure and activate circadian with the correct themes."
  (require 'circadian)
  (setq circadian-themes (cae-theme--get-circadian-config))
  ;; Ensure circadian recalculates and applies the theme now
  (circadian-setup))

;;;###autoload
(defun cae-theme--update-circadian-on-location-change ()
  "Hook function to reconfigure circadian when location changes significantly."
  (when (and (featurep 'circadian) cae-theme-enable-day-night-theme-switching)
    (message "Theme: Location changed, reconfiguring circadian.")
    (cae-theme--configure-circadian)))
