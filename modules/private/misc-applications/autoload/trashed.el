;;; private/misc-applications/autoload/trashed.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +trashed-revert-buffer-a (oldfun)
  (when (prog1 (buffer-live-p trashed-buffer)
          (funcall oldfun))
    (revert-buffer)))

;;;###autoload
(defun +trashed-hide-cursor-h ()
  (setq-local cursor-type nil)
  (when (featurep 'evil)
    (setq-local evil-normal-state-cursor '(bar . 0)
                evil-insert-state-cursor '(bar . 0)
                evil-visual-state-cursor '(box . 0)
                evil-motion-state-cursor '(box . 0)
                evil-replace-state-cursor '(hbar . 0)
                evil-operator-state-cursor '(hbar . 0)
                evil-emacs-state-cursor '(hbar . 0))))

;;;###autoload (autoload '+trashed-hydra/body "private/misc-applications/autoload/trashed" nil t)
(defhydra +trashed-hydra (:color pink)
  ("<f6>" nil "Exit" :exit t)
  ("D" trashed-do-delete "Delete" :column "Do")
  ("R" trashed-do-restore "Restore" :column "Do")
  ("e" trashed-browse-url-of-file "Browse URL of file" :column "View")
  ("v" trashed-view-file "View file" :column "View")
  ("d" trashed-flag-delete "Flag delete" :column "Flag")
  ("r" trashed-do-restore-and-exit "Flag restore" :column "Flag")
  ("f" trashed-find-file "Find file" :column "View")
  ("o" trashed-display-file "Find file other window" :column "View")
  ("C-o" trashed-display-file "Display file" :column "View")
  ("m" trashed-mark "Mark" :column "Mark")
  ("t" trashed-toggle-marks "Toggle marks" :column "Mark")
  ("U" trashed-unmark-all "Unmark all" :column "Mark")
  ("S" tabulated-list-sort "Sort" :column "Misc")
  ("g" revert-buffer "Refresh" :column "Misc")
  ("#" trashed-flag-auto-save-files "Flag auto save files" :column "Mark")
  ("x" trashed-do-execute "Execute" :column "Do")
  ("~" trashed-flag-backup-files "Flag backup files for deletion" :column "Flag")
  ("$ d" trashed-flag-delete-files-by-date "Flag delete by date" :column "Flag")
  ("$ m" trashed-mark-files-by-date "Mark by date" :column "Mark")
  ("$ r" trashed-flag-restore-files-by-date "Flag restore by date" :column "Flag")
  ("$ u" trashed-unmark-files-by-date "Unmark by date" :column "Mark")
  ("% d" trashed-flag-delete-files-regexp "Flag delete by regexp" :column "Flag")
  ("% m" trashed-mark-files-regexp "Mark by regexp" :column "Mark")
  ("% r" trashed-flag-restore-files-regexp "Flag restore by regexp" :column "Flag")
  ("% u" trashed-unmark-files-regexp "Unmark by regexp" :column "Mark"))

;;;###autoload
(defun +trashed ()
  "TODO I should PR this to trashed.el as a replacement for `trashed'."
  (interactive)
  (if (not (buffer-live-p trashed-buffer))
      (progn
        (pop-to-buffer
         (setq trashed-buffer (get-buffer-create trashed-buffer-name)))
        (with-current-buffer trashed-buffer
          (trashed-mode)
          (revert-buffer)
          (trashed-reset-vpos)
          (trashed-reset-hpos t))))
  (pop-to-buffer trashed-buffer))
