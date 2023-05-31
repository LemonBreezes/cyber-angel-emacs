;;; private/dired/autoload.el -*- lexical-binding: t; -*-

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

;;;###autoload
