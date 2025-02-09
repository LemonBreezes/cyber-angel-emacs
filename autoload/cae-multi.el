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
(defun cae-multi-pull-repositories (&optional verb-level)
  "Pull the shared repositories and handle conflicts asynchronously.
  
VERB-LEVEL controls output:
  0: Silent (errors only; if explicitly set)
  1: Normal messages (default when called interactively)
  2: Extra verbose output
  
When called interactively, no prefix yields level 1 and a prefix yields level 2."
  (interactive (list (if current-prefix-arg 2 1)))
  (let ((output-buffer (get-buffer-create " *cae-multi-pull-repositories*"))
        (all-pulls-succeeded t)
        (pending-processes 0))
    (with-current-buffer output-buffer (erase-buffer))
    (dolist (repo-dir cae-multi-repositories)
      (let ((default-directory repo-dir))
        (when (file-directory-p (concat repo-dir "/.git"))
          (if (file-exists-p (concat repo-dir "/.git/index.lock"))
              (when (>= verb-level 1)
                (message "Git lockfile exists in %s, skipping pull" repo-dir))
            (setq pending-processes (1+ pending-processes))
            (let ((process (start-process
                            "git-pull-process"
                            output-buffer
                            "git" "pull" "--recurse-submodules=on-demand")))
              (set-process-sentinel
               process
               (lambda (proc event)
                 (when (memq (process-status proc) '(exit signal))
                   (if (/= (process-exit-status proc) 0)
                       (progn
                         (message "Git pull failed in %s" repo-dir)
                         (with-current-buffer output-buffer
                           (goto-char (point-max))
                           (insert (format "\nError: Git pull failed in repository %s\n" repo-dir)))
                         (display-buffer output-buffer)
                         (setq all-pulls-succeeded nil))
                     (progn
                       (when (>= verb-level 1)
                         (message "Git pull succeeded in %s" repo-dir))
                       (setq pending-processes (1+ pending-processes))
                       (let ((submodule-process
                              (start-process "git-submodule-update-process"
                                             output-buffer
                                             "git" "submodule" "update" "--init" "--recursive")))
                         (set-process-sentinel
                          submodule-process
                          (lambda (subproc subevent)
                            (when (memq (process-status subproc) '(exit signal))
                              (if (/= (process-exit-status subproc) 0)
                                  (progn
                                    (message "Git submodule update failed in %s" repo-dir)
                                    (with-current-buffer output-buffer
                                      (goto-char (point-max))
                                      (insert (format "\nError: Git submodule update failed in repository %s\n" repo-dir)))
                                    (display-buffer output-buffer)
                                    (setq all-pulls-succeeded nil))
                                (when (>= verb-level 1)
                                  (message "Git submodule update succeeded in %s" repo-dir)))
                              (with-current-buffer output-buffer
                                (save-excursion
                                  (goto-char (point-max))
                                  (if (re-search-backward "\\bCONFLICT\\b" nil t)
                                      (progn
                                        (message "Conflict detected during git submodule update in %s" repo-dir)
                                        (display-buffer output-buffer)
                                        (setq all-pulls-succeeded nil))
                                    (when (>= verb-level 2)
                                      (message "Submodules updated successfully in %s" repo-dir)))))
                              (setq pending-processes (1- pending-processes))
                              (when (zerop pending-processes)
                                (if all-pulls-succeeded
                                    (cae-multi--run-doom-sync verb-level)
                                  (message "One or more git operations failed. See %s for details" (buffer-name output-buffer)))))))))
                   (setq pending-processes (1- pending-processes))
                   (when (zerop pending-processes)
                     (if all-pulls-succeeded
                         (cae-multi--run-doom-sync verb-level)
                       (message "One or more git operations failed. See %s for details" (buffer-name output-buffer))))))))))))

(defun cae-multi--run-doom-sync (verb-level)
  "Run 'doom sync' asynchronously and redirect output to the output buffer.
VERB-LEVEL controls how much output is emitted."
  (let* ((buffer-name " *cae-multi-pull-repositories*")
         (process
          (start-process
           "doom-sync-process"
           buffer-name
           "doom" "sync")))
    (set-process-sentinel
     process
     (lambda (proc event)
       (when (memq (process-status proc) '(exit signal))
         (if (= (process-exit-status proc) 0)
             (when (>= verb-level 1)
               (message "'doom sync' finished successfully"))
           (message "'doom sync' failed with exit code %d" (process-exit-status proc))
           ;; Optionally display the output buffer
           (display-buffer buffer-name)))))))
