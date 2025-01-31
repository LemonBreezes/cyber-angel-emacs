;;; autoload/cae-multi.el -*- lexical-binding: t; -*-

(defun cae-multi-commit-file (file)
  (when (file-in-directory-p file doom-user-dir)
    (let ((gac-automatically-push-p t)
          (gac-silent-message-p t))
      (gac--after-save (find-file-noselect file)))))

;;;###autoload
(defun cae-multi-bookmark-push-changes-a (&rest _)
  (gac--after-save bookmark-default-file))

;;;###autoload
(defun cae-multi-org-archive-push-changes-h ()
  (gac--after-save (buffer-file-name))
  (dolist (file (org-all-archive-files))
    (gac--after-save file)))

;;;###autoload
(defun cae-multi-abbrev-write-file-a (orig-fun &optional file verbose)
  (let ((mtime (nth 5 (file-attributes abbrev-file-name))))
    (if (or (null cae-multi-abbrev--file-mtime)
            (equal mtime cae-multi-abbrev--file-mtime))
        (progn (funcall orig-fun file verbose)
               (setq cae-multi-abbrev--file-mtime mtime)
               (unless file
                 (if cae-multi-abbrev--auto-commit-disabled
                     (when (y-or-n-p
                            (concat "Abbrev file modified since a previous "
                                    "save. Enable auto-commit?"))
                       (setq cae-multi-abbrev--auto-commit-disabled nil)
                       (cae-multi-commit-file abbrev-file-name))
                   (cae-multi-commit-file abbrev-file-name))))
      (message (concat "Abbrev file modified since last save. "
                       "Disabling abbrev file auto-commit."))
      (funcall orig-fun file verbose)
      (setq cae-multi-abbrev--file-mtime mtime
            cae-multi-abbrev--auto-commit-disabled t))))

;;;###autoload
(defun cae-multi-pull-repositories ()
  "Pull the shared repositories and handle conflicts asynchronously."
  (interactive)
  (let ((output-buffer (get-buffer-create " *cae-multi-pull-repositories*"))
        (all-pulls-succeeded t)
        (processes '()))
    (dolist (repo-dir cae-multi-repositories)
      (let ((default-directory repo-dir))
        (when (file-directory-p (concat repo-dir "/.git"))
          (if (file-exists-p (concat repo-dir "/.git/index.lock"))
              (message "Git lockfile exists in %s, skipping pull" repo-dir)
            (let ((process
                   (start-process
                    "git-pull-process"
                    output-buffer
                    "git" "pull" "--recurse-submodules=on-demand")))
              (push process processes)
              (set-process-sentinel
               process
               (lambda (proc event)
                 (when (memq (process-status proc) '(exit signal))
                   (if (/= (process-exit-status proc) 0)
                       (progn
                         (message "Git pull failed in %s" repo-dir)
                         ;; Optionally display the output buffer
                         (display-buffer output-buffer)

                         (with-current-buffer output-buffer
                           (save-excursion
                             (goto-char (point-max))
                             (if (re-search-backward "CONFLICT" nil t)
                                 (progn
                                   (message "Conflict detected during git pull in %s" repo-dir)
                                   (display-buffer output-buffer)
                                   (setq all-pulls-succeeded nil))
                               (message "Git pull succeeded in %s" repo-dir))
                             ;; Start git submodule update --init --recursive
                             (let ((submodule-process
                                    (start-process
                                     "git-submodule-update-process"
                                     output-buffer
                                     "git" "submodule" "update" "--init" "--recursive")))
                               (push submodule-process processes)
                               (set-process-sentinel
                                submodule-process
                                (lambda (proc event)
                                  (when (memq (process-status proc) '(exit signal))
                                    (if (/= (process-exit-status proc) 0)
                                        (progn
                                          (message "Git submodule update failed in %s" repo-dir)
                                          ;; Optionally display the output buffer
                                          (display-buffer output-buffer)
                                          (setq all-pulls-succeeded nil))
                                      (message "Git submodule update succeeded in %s" repo-dir))
                                    ;; Check for conflicts in submodule update
                                    (with-current-buffer output-buffer
                                      (save-excursion
                                        (goto-char (point-max))
                                        (if (re-search-backward "CONFLICT" nil t)
                                            (progn
                                              (message "Conflict detected during git submodule update in %s" repo-dir)
                                              (display-buffer output-buffer)
                                              (setq all-pulls-succeeded nil))
                                          (message "Submodules updated successfully in %s" repo-dir))))
                                    ;; When all processes have finished, run 'doom sync' if needed
                                    (when (and (null (delq proc (cl-remove-if #'process-live-p processes)))
                                               all-pulls-succeeded)
                                      (cae-multi--run-doom-sync))))))))
                         ;; When all processes have finished, run 'doom sync' if needed
                         (when (and (null (delq proc (cl-remove-if #'process-live-p processes)))
                                    all-pulls-succeeded)
                           (cae-multi--run-doom-sync))))))))))))))
(defun cae-multi--run-doom-sync ()
  "Run 'doom sync' asynchronously and redirect output to the output buffer."
  (let ((process
         (start-process
          "doom-sync-process"
          " *cae-multi-pull-repositories*"
          "doom" "sync")))
    (set-process-sentinel
     process
     (lambda (proc event)
       (when (memq (process-status proc) '(exit signal))
         (if (= (process-exit-status proc) 0)
             (message "'doom sync' finished successfully")
           (message "'doom sync' failed with exit code %d" (process-exit-status proc))
           ;; Optionally display the output buffer
           (display-buffer (process-buffer process))))))))
