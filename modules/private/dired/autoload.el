;;; private/dired/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-dired-load-dirvish-h (dir)
  (remove-hook 'find-directory-functions #'cae-dired-load-dirvish-h)
  (require 'dirvish nil t)
  (unless (memq #'dired-noselect find-directory-functions)
    (add-hook 'find-directory-functions #'dired-noselect t))
  (dired-noselect dir))

;;;###autoload
(defun cae-dired-find-file-a (file &rest _)
  "Like `find-file', but might exit the current Dirvish session."
  (if (derived-mode-p 'dired-mode)
      (progn
        (when-let ((dir (file-name-directory file)))
          (unless (file-equal-p dir default-directory)
            (advice-remove #'find-file #'cae-dired-find-file-a)
            (unwind-protect
                (find-file dir)
              (advice-add #'find-file :before #'cae-dired-find-file-a))))
        (unless (file-directory-p file)
          ;; Copied from `dirvish-find-entry-a'
          (let* ((dv (dirvish-curr)) (fn (nth 4 (dv-type dv))))
            (if fn (funcall fn) (dirvish-kill dv)))))))

(defun cae-dired-find-file-fullscreen-h ()
  (when (and (derived-mode-p 'dired-mode)
             (one-window-p))
    (ignore-error user-error
      (dirvish-layout-switch dirvish-default-layout))))

(defun cae-dired-find-file-other-window-a (&rest _)
  (when (and (derived-mode-p 'dired-mode)
             (window-dedicated-p))
    (dirvish-quit)))

(defun cae-dired-switch-buffer--handle-dirvish (fn)
  (when (and (derived-mode-p 'dired-mode)
             (window-dedicated-p))
    (dirvish-layout-toggle))
  (call-interactively fn)
  (when (and (derived-mode-p 'dired-mode)
             (one-window-p)
             (not (window-dedicated-p)))
    (ignore-error user-error
      (dirvish-layout-switch dirvish-default-layout))))

;;;###autoload
(defun cae-dired-previous-buffer ()
  (interactive)
  (cae-dired-switch-buffer--handle-dirvish #'previous-buffer))

;;;###autoload
(defun cae-dired-next-buffer ()
  (interactive)
  (cae-dired-switch-buffer--handle-dirvish #'next-buffer))

;;;###autoload
(defun cae-dired-jump ()
  (interactive)
  (call-interactively #'dired-jump)
  (when (one-window-p)
    (ignore-error user-error
      (dirvish-layout-switch dirvish-default-layout))))

;;;###autoload
(defun cae-dired-maximize-buffer ()
  (interactive)
  (call-interactively #'doom/window-maximize-buffer)
  (when (one-window-p)
    (ignore-error user-error
      (dirvish-layout-switch dirvish-default-layout))))
