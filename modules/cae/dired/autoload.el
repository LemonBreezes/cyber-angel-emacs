;;; cae/dired/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-dired-jump ()
  (interactive)
  (call-interactively #'dired-jump)
  (when (and (one-window-p)
             (dirvish-curr)
             (window-valid-p (dv-root-window (dirvish-curr))))
    (ignore-error user-error
      (dirvish-layout-switch dirvish-default-layout))))

;;;###autoload
(defun cae-dired-find-file-a (oldfun file &optional wildcards)
  "Like `find-file', but might exit the current Dirvish session."
  (interactive
   ;; Get file or buffer name to open
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  ;; Check if in Dired mode
  (if (and (derived-mode-p 'dired-mode)
           ;; This currently doesn't work with remote files. I will fix the code
           ;; for them when I have a use case.
           (not (file-remote-p file)))
      (progn
        ;; Check if file is in a different directory and if so change to it
        (when-let* ((dir (file-name-directory file)))
          (unless (file-equal-p dir default-directory)
            (funcall oldfun dir)))
        ;; If not a directory, kill Dirvish and find the file
        (unless (file-directory-p file)
          ;; Copied from `dirvish-find-entry-a'
          (let* ((dv (dirvish-curr)) (fn (nth 4 (dv-type dv))))
            (if fn (funcall fn) (dirvish-kill dv)))
          (funcall oldfun file wildcards)))
    ;; If not in Dired mode, find the file as usual
    (funcall oldfun file wildcards)))


;;;###autoload
(defun cae-dired-find-file-other-window-a (oldfun &rest args)
  (when (and (derived-mode-p 'dired-mode)
             (window-dedicated-p))
    (let* ((dv (dirvish-curr)) (fn (nth 4 (dv-type dv))))
      (if fn (funcall fn) (dirvish-kill dv))))
  (apply oldfun args))


;;;###autoload
(defun cae-dired-consult-jump-a (oldfun pos)
  ;; If any of the windows are dedicated dired-mode windows, kill Dirvish
  (cl-loop for win in (window-list)
           when (and (window-live-p win)
                     (window-dedicated-p win)
                     (parent-mode-is-derived-p
                      (buffer-local-value 'major-mode (window-buffer win))
                      'dired-mode))
           do (progn (with-selected-window win
                       (let* ((dv (dirvish-curr)) (fn (nth 4 (dv-type dv))))
                         (if fn (funcall fn) (dirvish-kill dv))))
                     (set-buffer (window-buffer)))
           finally return nil)
  (funcall oldfun pos)
  ;;(if (derived-mode-p 'dired-mode)
  ;;    (when-let* ((file
  ;;                (cond ((and (consp pos)
  ;;                            (markerp (car pos)))
  ;;                       (buffer-file-name (marker-buffer (car pos))))
  ;;                      ((stringp pos)
  ;;                       pos)
  ;;                      ((buffer-file-name
  ;;                        (marker-buffer pos))))))
  ;;      ;; Check if file is in a different directory and if so change to it
  ;;      (when-let* ((dir (file-name-directory file))
  ;;                 (_ (not (file-equal-p dir default-directory))))
  ;;        (find-file dir))
  ;;      ;; If not a directory, kill Dirvish and find the file
  ;;      (unless (file-directory-p file)
  ;;        ;; Copied from `dirvish-find-entry-a'
  ;;        (let* ((dv (dirvish-curr)) (fn (nth 4 (dv-type dv))))
  ;;          (if fn (funcall fn) (dirvish-kill dv)))
  ;;        (funcall oldfun pos)))
  ;;  ;; If not in Dired mode, find the file as usual
  ;;  )
  )

;;;###autoload
(defun cae-dired-hide-details-in-narrow-screen-h ()
  (when (<= (frame-width) 120)
    (dired-hide-details-mode 1)))
