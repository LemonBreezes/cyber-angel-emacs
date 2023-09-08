;;; private/dired/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-dired-jump ()
  (interactive)
  (call-interactively #'dired-jump)
  (when (and (one-window-p)
             (dirvish-curr)
             (window-valid-p (dv-root-window (dirvish-curr))))
    (ignore-error user-error
      (dirvish-layout-switch dirvish-default-layout))))

(defun cae-dired-dirvish-emerge-menu ()
  (interactive)
  (dirvish-emerge-mode +1)
  (call-interactively #'dirvish-emerge-menu))

;;;###autoload
(defun cae-dired-find-file-a (oldfun file &optional wildcards)
  "Like `find-file', but might exit the current Dirvish session."
  (interactive
   ;; Get file or buffer name to open
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  ;; Check if in Dired mode
  (if (derived-mode-p 'dired-mode)
      (progn
        ;; Check if file is in a different directory and if so change to it
        (when-let ((dir (file-name-directory file)))
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
  (if (derived-mode-p 'dired-mode)
      (when-let ((file
                  (cond ((and (consp pos)
                              (markerp (car pos)))
                         (buffer-file-name (marker-buffer (car pos))))
                        ((stringp pos)
                         pos)
                        ((buffer-file-name
                          (marker-buffer pos))))))
        ;; Check if file is in a different directory and if so change to it
        (when-let ((dir (file-name-directory file)))
          (unless (file-equal-p dir default-directory)
            (find-file dir)))
        ;; If not a directory, kill Dirvish and find the file
        (unless (file-directory-p file)
          ;; Copied from `dirvish-find-entry-a'
          (let* ((dv (dirvish-curr)) (fn (nth 4 (dv-type dv))))
            (if fn (funcall fn) (dirvish-kill dv)))
          (funcall oldfun pos)))
    ;; If not in Dired mode, find the file as usual
    (funcall oldfun pos)))
