;;; private/dired/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-dired-jump ()
  (interactive)
  (call-interactively #'dired-jump)
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
  ("M-j" dirvish-fd-jump "Jump to directory" :column "Navigate")
  ("M-k" dirvish-fd "fd on current directory" :column "Navigate")
  ("M-f" dirvish-history-go-forward "go forward in history" :column "Navigate")
  ("M-b" dirvish-history-go-backward "go backward in history" :column "Navigate")
  ("M-i" dirvish-fd-switches-menu "Set fd switches" :column "UI")
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
  (when (consp pos) (setq pos (car pos)))
  (when (and (markerp pos)
             (derived-mode-p 'dired-mode))
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
            (funcall oldfun dir)))
        ;; If not a directory, kill Dirvish and find the file
        (unless (file-directory-p file)
          ;; Copied from `dirvish-find-entry-a'
          (let* ((dv (dirvish-curr)) (fn (nth 4 (dv-type dv))))
            (if fn (funcall fn) (dirvish-kill dv)))
          (funcall oldfun pos))))
  ;; If not in Dired mode, find the file as usual
  (funcall oldfun pos))
