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

;;;###autoload (autoload 'cae-dired-dirvish-hydra/body "private/dired/autoload" nil t)
(defhydra cae-dired-dirvish-hydra (:color blue)
  ("<f6>" nil "Exit" :exit t)
  ("M-e" dirvish-emerge-menu "Emerge menu" :column "Menu")
  ("M-l" dirvish-ls-switches-menu "ls menu" :column "Menu")
  ("M-m" dirvish-mark-menu "Mark menu" :column "Menu")
  ("M-s" dirvish-setup-menu "Setup menu" :column "Menu")
  ("f" dirvish-file-info-menu "File info menu" :column "Menu")
  ("v" dirvish-vc-menu "VC menu" :column "Menu")
  ("y" dirvish-yank-menu "Yank menu" :column "Menu")
  ("a" dirvish-quick-access "Quick access" :column "Menu")
  ("s" dirvish-quicksort "Quicksort" :column "Menu")
  ("?" dirvish-dispatch "Dirvish dispatch" :column "Menu"))
