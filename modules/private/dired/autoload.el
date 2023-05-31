;;; private/dired/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-dired-load-dirvish-h (dir)
  (remove-hook 'find-directory-functions #'cae-dired-load-dirvish-h)
  (require 'dirvish nil t)
  (unless (memq #'dired-noselect find-directory-functions)
    (add-hook 'find-directory-functions #'dired-noselect t))
  (dired-noselect dir)
  (when (and (frame-width (selected-frame))
             (> (frame-width (selected-frame)) 160))
    (dirvish-dwim)))

;;;###autoload
(defun cae-dired-find-file-a (oldfun file &optional wildcards)
  "Like `find-file', but might exit the current Dirvish session."
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (if (derived-mode-p 'dired-mode)
      (progn
        (when-let ((dir (file-name-directory file)))
          (unless (file-equal-p dir default-directory)
            (funcall oldfun dir)))
        (unless (file-directory-p file)
          ;; Copied from `dirvish-find-entry-a'
          (let* ((dv (dirvish-curr)) (fn (nth 4 (dv-type dv))))
            (if fn (funcall fn) (dirvish-kill dv)))
          (funcall oldfun file wildcards)))
    (funcall oldfun file wildcards)
    (when (and (file-directory-p file)
               (frame-width (selected-frame))
               (> (frame-width (selected-frame)) 160))
      (dirvish-dwim))))

(defun cae-dired-switch-buffer--handle-dirvish ()
  (when (and (featurep 'dirvish)
             (dirvish-curr)
             (window-dedicated-p))
    (dirvish-layout-toggle)))

;;;###autoload
(defun cae-dired-previous-buffer ()
  (interactive)
  (cae-dired-switch-buffer--handle-dirvish)
  (call-interactively #'previous-buffer))

;;;###autoload
(defun cae-dired-next-buffer ()
  (interactive)
  (cae-dired-switch-buffer--handle-dirvish)
  (call-interactively #'next-buffer))
