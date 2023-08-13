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
(defhydra cae-dired-dirvish-hydra (:color pink)
  ("<f6>" nil "Exit" :exit t)
  ("M-e" cae-dired-dirvish-emerge-menu "Emerge menu" :column "UI")
  ("M-l" dirvish-ls-switches-menu "ls menu" :column "UI")
  ("M-m" dirvish-mark-menu "Mark menu" :column "Misc")
  ("M-r" dirvish-rsync-transient "Rsync menu" :column "Act")
  ("M-c" dirvish-chxxx-menu "ChXXX menu" :column "Act")
  ("M-h" dirvish-history-menu "History menu" :column "Navigate")
  ("M-;" dirvish-epa-dired-menu "EPA menu" :column "Act")
  ("M-s" dirvish-setup-menu "Setup menu" :column "UI")
  ("f" dirvish-file-info-menu "File info menu" :column "Act")
  ("v" dirvish-vc-menu "VC menu" :column "UI")
  ("y" dirvish-yank-menu "Yank menu" :column "Act")
  ("a" dirvish-quick-access "Quick access" :column "Navigate")
  ("s" dirvish-quicksort "Quicksort" :column "UI")
  ("?" dirvish-dispatch "Dirvish dispatch" :column "Misc"))

(defun cae-dired-dirvish-emerge-menu ()
  (interactive)
  (dirvish-emerge-mode +1)
  (call-interactively #'dirvish-emerge-menu))

(defun cae-dired-find-file-a (oldfun &rest args)
  "Like `find-file', but might exit the current Dirvish session."
  (if-let* ((file (and (eq major-mode 'dired-mode)
                       (car args)))
            (dir (file-name-directory file)))
      (progn
        (unless (file-equal-p dir default-directory)
          (advice-remove #'find-file #'cae-dired-find-file-a)
          (unwind-protect
              (find-file dir)
            (advice-add #'find-file :before #'cae-dired-find-file-a)))
        (unless (file-directory-p file)
          ;; Copied from `dirvish-find-entry-a'
          (let* ((dv (dirvish-curr)) (fn (nth 4 (dv-type dv))))
            (if fn (funcall fn) (dirvish-kill dv)))))
    (apply oldfun args)))

;;(defun cae-dired-find-file-fullscreen-h ()
;;  (when (and (derived-mode-p 'dired-mode)
;;             (one-window-p))
;;    (ignore-error user-error
;;      (dirvish-layout-switch dirvish-default-layout))))
;;
;;(defun cae-dired-find-file-other-window-a (&rest _)
;;  (when (and (derived-mode-p 'dired-mode)
;;             (window-dedicated-p))
;;    (dirvish-quit)))
