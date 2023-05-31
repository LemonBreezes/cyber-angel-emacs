;;; private/unpackaged/autoload/magit.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cae-unpackaged-magit-status ()
  "Open a `magit-status' buffer and close the other window so only Magit is visible.
If a file was visited in the buffer that was active when this
command was called, go to its unstaged changes section."
  (interactive)
  (let* ((buffer-file-path (when buffer-file-name
                             (file-relative-name buffer-file-name
                                                 (locate-dominating-file buffer-file-name ".git"))))
         (section-ident `((file . ,buffer-file-path) (unstaged) (status))))
    (call-interactively #'magit-status)
    (delete-other-windows)
    (when buffer-file-path
      (goto-char (point-min))
      (cl-loop until (when (equal section-ident (magit-section-ident (magit-current-section)))
                       (magit-section-show (magit-current-section))
                       t)
               do (condition-case nil
                      (magit-section-forward)
                    (error (cl-return (magit-status-goto-initial-section-1))))))
    (recenter)
    ;; It's a good time to garbage collect, since I spend a little time reading the status buffer.
    (cae-hacks-garbage-collect)))

;;;###autoload
(defun cae-unpackaged-magit-save-buffer-show-status-here ()
  "Like `cae-unpackaged-magit-save-buffer-show-status' but with
non-nil `magit-status-goto-file-position'."
  (interactive)
  (when (buffer-file-name)
    (save-buffer))
  (let ((magit-status-goto-file-position t))
    (call-interactively #'magit-status))
  (delete-other-windows)
  (recenter)
  (cae-hacks-garbage-collect))

;;;###autoload
(defun cae-unpackaged-magit-save-buffer-show-status ()
  "Save buffer and show its changes in `magit-status'."
  (interactive)
  (when (buffer-file-name)
    (save-buffer))
  (cae-unpackaged-magit-status))

;; Do not jump to changes if they have been automatically commited.
(defun cae-unpackaged-magit-status-disable-when-gac-enabled-a ()
  (and (eq this-command 'unpackaged/magit-save-buffer-show-status)
       (or (bound-and-true-p git-auto-commit-mode)
           (not (doom-project-root))
           (not (magit-git-repo-p (doom-project-root)))
           (not (buffer-file-name))
           (not (magit-file-tracked-p (buffer-file-name))))))
